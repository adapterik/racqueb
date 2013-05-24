#lang racket

(require racket/dict
         (file "/home/epearson/work/racket/github/racket-couchdb/main.rkt"))

(provide assoc-path dict-path content-database btos)
(provide read-bytes parse-content-type read-status-line read-header)

(define (assoc-path path assoc-list (default-value #f))
  (letrec ([r (λ (p l)
                (if l
                    (let ([res (assoc (car p) l)])
                      (if res
                          (if (null? (cdr p))
                              (cdr res)
                              (r (cdr p) (cdr res)))
                          default-value))
                    default-value))])
    (if (symbol? path)
        (r (list path) assoc-list)
        (r path assoc-list))))

(define (dict-path dict path)
  (letrec ([r (λ (p d)
                (and d
                     (match p
                       [(list) d]
                       [(list-rest prop rest)
                        (r rest (dict-ref d prop #f))])))])
    (r path dict)))

(define (content-database info)
  (let* ([datasource (assoc-path '(datasource content) (hash-ref info 'vhost))]
         [connection (couchdb-connect #:host (assoc-path 'host datasource))])
    (couchdb-db connection (assoc-path 'database datasource))))

(define (btos bytes)
  (bytes->string/utf-8 bytes))


;;; ON shim for the http lib. need to address with with the racket folks.

(define re-header-field #px#"([^:\\s]*)[\\s]*:[\\s]*([^\r\n]*)[\r][\n]?")

(define re-status-line #px#"[Hn][Tt][Tt][Pp]/([^ ]*)[\\s]+([^\\s]+)[\\s]*([^\r\n]*)\r\n")

(define re-header-field-value #px#"[\\s]*([^;$]+)")

;; maybe spaces, then semicolon, then something not a space or equals,
;; then maybe some spaces, then an equals sign, then maybe some spaces,
;; then something not a sapce or semicolon or end of line.
(define re-header-field-param #px#"[\\s]*;[\\s]*([^\\s=]+)[\\s]*=[\\s]*([^\\s;$]+)")

(define (parse-content-type s)
  (let ([in (open-input-bytes s)])
    ;; extract value
    (match (regexp-match re-header-field-value in)
      [(list _ value)
       (cons value (let loop ([params '()])
                     (match (regexp-match re-header-field-param in)
                       [#f (reverse params)]
                       [(list _ name value)
                        (loop (cons (cons name value) params))])))])))

(define (read-status-line input)
  (match (regexp-match re-status-line input)
    [(list _ version status message)
     (list version status message)]))
  
(define (read-header in)
  (letrec ([r (λ (accum)
                (match (regexp-match re-header-field in)
                  (#f (reverse accum))
                  ((list _ name value)
                   (r (cons (cons (string->symbol
                                   (string-downcase
                                    (bytes->string/utf-8 name))) value) accum)))))])
    (r '())))

(define (read-bytes in)
  (let ([bout (open-output-bytes)])
    (letrec ([r (λ () 
                   (let ([b (read-byte in)])
                     (unless (eof-object? b)
                       (write-byte b bout)
                       (r))))])
      (r)
      (get-output-bytes bout))))
