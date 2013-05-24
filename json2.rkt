#lang racket/base

#| Roughly based on the PLaneT package by Dave Herman,
   Originally released under MIT license.
EAP - 4/20/2013 - change the object key from symbol to string,
cleaned up code, replace some single-letter variables with more descriptive,
simplify some functions by moving local functions out, moving multiple defines into 
a single let.

|#

(provide jsexpr? write-json read-json jsexpr->string jsexpr->bytes string->jsexpr bytes->jsexpr)


;; ----------------------------------------------------------------------------
;; Customization


;; ----------------------------------------------------------------------------
;; Predicate

(define (jsexpr? x)
  (let loop ([x x])
    (or (string? x)
        (and (list? x) (andmap loop x))
        (and (hash? x) (for/and ([(k v) (in-hash x)])
                         (and (string? k) (loop v))))        
        (exact-integer? x)
        (inexact-real? x)
        ;; (boolean? x)
        (or (eq? #true) (eq? #false)        
        (eq? x 'null)))))

;; ----------------------------------------------------------------------------
;; Generation: Racket -> JSON

(define json-char-map '([#\backspace . "\\b"] [#\newline . "\\n"] [#\return . "\\r"]
                                              [#\page . "\\f"] [#\tab . "\\t"]
                                              [#\\ . "\\\\"] [#\" . "\\\""]))

(define (u-esc n)
  (define str (number->string n 16))
  (define pad (case (string-length str)
                [(1) "000"] [(2) "00"] [(3) "0"] [else ""]))
  (string-append "\\u" pad str))

(define (escape-char m)
  (let ([ch (string-ref m 0)])
    (or (assoc ch json-char-map)
        (let ([n (char->integer ch)])
          (if (< n #x10000)
              (u-esc n)
              ;; use the (utf-16 surrogate pair) double \u-encoding
              (let ([n (- n #x10000)])
                (string-append (u-esc (+ #xD800 (arithmetic-shift n -10)))
                               (u-esc (+ #xDC00 (bitwise-and n #x3FF))))))))))

(define (write-json-string str out #:encode [enc'control])
  (let ([rx-to-encode (case enc
                        [(control) #rx"[\0-\37\\\"\177]"]
                        [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]
                        [else (raise-type-error 'write-json "encoding symbol" enc)])])
    (write-bytes #"\"" out)
    (write-string (regexp-replace* rx-to-encode str escape-char) out)
    (write-bytes #"\"" out)))
 
   
(define (write-json json-term [json-out (current-output-port)]
                    #:encode [enc 'control])
    (let loop ([json-term json-term])
      (cond 
        ;; Number
        [(or (exact-integer? json-term) (inexact-real? json-term)) (write json-term json-out)]
        ;; Boolean
        [(eq? json-term #false) (write-bytes #"false" json-out)]
        [(eq? json-term #true) (write-bytes #"true" json-out)]
        ;; Null
        [(eq? json-term 'null) (write-bytes #"null" json-out)]
        ;; String
        [(string? json-term) (write-json-string json-term json-out #:encode enc)]
        ;; Array
        [(list? json-term) (write-bytes #"[" json-out)
                           (for ([array-item (in-list json-term)]
                                 [i (in-naturals)])
                             (and (> i 0) (write-bytes #"," json-out))
                             (loop array-item))
                           (write-bytes #"]" json-out)]
        ;; Object
        [(hash? json-term) (write-bytes #"{" json-out)
                           (for ([(k v) (in-hash json-term)]
                                 [i (in-naturals)])
                             (unless (string? k)
                               (raise-type-error 'write-json "legal JSON key value" k))
                             (and (> i 0) (write-bytes #"," json-out))
                             (write k json-out) ; no `printf' => proper escapes
                             (write-bytes #":" json-out)
                             (loop v))
                           (write-bytes #"}" json-out)]
        ;; Should never get here
        [else (raise-type-error 'write-json "legal JSON value" json-term)]))
    (void))
  
  ;; ----------------------------------------------------------------------------
;; Parsing: JSON -> Racket

(require syntax/readerr)


(define escapable '([#"b" . #"\b"] [#"n" . #"\n"] [#"r" . #"\r"]
                                   [#"f" . #"\f"] [#"t" . #"\t"]
                                   [#"\\" . #"\\"] [#"\"" . #"\""] [#"/" . #"/"]))

(define (read-json [json-input (current-input-port)])
  ;; Follows the specification (eg, at json.org) -- no extensions.
  ;;
  (define (err fmt . args)
    (define-values [l c p] (port-next-location json-input))
    (raise-read-error (format "read-json: ~a" (apply format fmt args))
                      (object-name json-input) l c p #f))
  
  (define (skip-whitespace) (regexp-match? #px#"^\\s*" json-input))
  ;;
  ;; Reading a string *could* have been nearly trivial using the racket
  ;; reader, except that it won't handle a "\/"...
  (define (read-string)
    (let loop ([accum* '()])
      ;; note: use a string regexp to extract utf-8-able text
      (let* ([matched (cdr (or (regexp-try-match #rx"^([^\"\\]*)(\"|\\\\(.))" json-input)
                               (err "unterminated string")))]
             ;; Here we fix up the accum by pushing any string we find before the " or \.
             [accum (if (> (bytes-length (car matched)) 0) (cons (car matched) accum*) accum*)]
             [esc (caddr matched)])
        (cond
          ;; Terminating condition.
          ;; No escaping, convert our accum into a string and return it. This means that we ended the regexp
          ;; above with just matching on the end of string ".
          [(not esc) (bytes->string/utf-8 (apply bytes-append (reverse accum)))]
          ;; If we find the escape char in our special list of json escape sequences, just pick up the
          ;; matching one.
          [(assoc esc escapable)
           => (λ (m) (loop (cons (cdr m) accum)))]
          ;; Otherwise, we might have a unicode escape sequence.
          [(equal? esc #"u")
           (let* ([matched (or (regexp-try-match #px#"^[a-fA-F0-9]{4}" json-input)
                               (err "bad string \\u escape"))]
                  [char-code (string->number (bytes->string/utf-8 (car matched)) 16)]
                  [unicode-char-code (if (<= #xD800 char-code #xDFFF)
                                         ;; it's the first part of a UTF-16 surrogate pair
                                         (let* ([matched (or (regexp-try-match #px#"^\\\\u([a-fA-F0-9]{4})" json-input)
                                                             (err "bad string \\u escape, ~a"
                                                                  "missing second half of a UTF16 pair"))]
                                                [char-code2 (string->number (bytes->string/utf-8 (cadr matched)) 16)])
                                           (if (<= #xDC00 char-code2 #xDFFF)
                                               (+ (arithmetic-shift (- char-code #xD800) 10) (- char-code2 #xDC00) #x10000)
                                               (err "bad string \\u escape, ~a"
                                                    "bad second half of a UTF16 pair")))
                                         char-code)]) ; single \u escape
             ;; NB looks inefficient to save as a bytes
             (loop (cons (string->bytes/utf-8 (string (integer->char unicode-char-code))) accum)))]
          [else (err "bad string escape: \"~a\"" esc)]))))
  ;;
  (define (read-array)
    (skip-whitespace)
    (if (regexp-try-match #rx#"^\\]" json-input)
                      '()
                      (let loop ([l (list (read-json))])
                        (skip-whitespace)
                        (cond [(regexp-try-match #rx#"^\\]" json-input) (reverse l)]
                              [(regexp-try-match #rx#"^," json-input) (loop (cons (read-json) l))]
                              [else (err "error while parsing a json array")]))))
  
  (define (read-object)
    (skip-whitespace)
    (make-hash 
     (if (regexp-try-match #rx#"^}" json-input) '()
         (let loop ([accum '()])
           (let ([k (read-json)])
             (unless (string? k)
               (err "Non-string value '~a' used for json object key" k))
             (skip-whitespace)
             (unless (regexp-try-match #rx#"^:" json-input)
               (err "Error while parsing a json object pair, expected ':'"))
             (let ([v (read-json)])
               ;; Make sure we have either the object termination or the next
               ;; pair marker (comma)
               (cond [(regexp-try-match #rx#"^}" json-input) (reverse (cons (cons k v) accum))]
                     [(regexp-try-match #rx#"^," json-input) (loop (cons (cons k v) accum))]
                     [else (err "error while parsing a json object, object pair not terminated with } or ,")])))))))
               
  ;;
  (define (read-json [top? #f])
    (skip-whitespace)
    (cond
      [(and top? (eof-object? (peek-char json-input))) eof]
      [(regexp-try-match #px#"^true\\b"  json-input) #true]
      [(regexp-try-match #px#"^false\\b" json-input) #false]
      [(regexp-try-match #px#"^null\\b"  json-input) 'null]
      [(regexp-try-match
        #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?" json-input)
       => (λ (bs) (string->number (bytes->string/utf-8 (car bs))))]
      [(regexp-try-match #rx#"^[\"[{]" json-input)
       => (λ (m)
            (let ([m (car m)])
              (cond [(equal? m #"\"") (read-string)]
                    [(equal? m #"[")  (read-array)]
                    [(equal? m #"{")  (read-object)])))]
      [else (err "bad input")]))
  ;;
  (read-json #t))

;; Accessing data.


;; ----------------------------------------------------------------------------
;; Convenience functions


(define (jsexpr->string x #:encode [enc 'control])
  (define o (open-output-string))
  (write-json x o #:encode enc)
  (get-output-string o))

(define (jsexpr->bytes x #:encode [enc 'control])
  (define o (open-output-bytes))
  (write-json x o #:encode enc)
  (get-output-bytes o))


(define (string->jsexpr str)
  (unless (string? str) (raise-type-error 'string->jsexpr "string" str))
  (read-json (open-input-string str)))

(define (bytes->jsexpr str)
  (unless (bytes? str) (raise-type-error 'bytes->jsexpr "bytes" str))
  (read-json (open-input-bytes str)))

