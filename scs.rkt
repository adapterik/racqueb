#lang racket

;; SCS implementation
;; http://tools.ietf.org/html/rfc6896

(require net/base64 (planet vyzo/crypto) (planet vyzo/crypto/util))

(provide scs-encode scs-decode scs-make-key test)

;; Private

(define (bytes->hex-string b)
  (hex b))

(define (bytes->hex-bytes b)
  (hex b))

(define (hex-string->bytes s)
  (letrec ([r (λ (in accum)
                (let ([next (read-string 2 in)])
                  (if (eof-object? next)
                      (list->string (reverse accum))
                      (r in (cons (integer->char (string->number next 16)) accum)))))])
    (r (open-input-string s) '())))

(define (hex-bytes->bytes s)
  (letrec ([r (λ (in accum)
                (let ([next (read-bytes 2 in)])
                  (if (eof-object? next)
                      (list->bytes (reverse accum))
                      (r in (cons (string->number (bytes->string/latin-1 next) 16) accum)))))])
    (r (open-input-bytes s) '()))) 
  

(define (bytes-encode b)
  ;; (base64-encode b)
  (bytes->hex-bytes b)
  )
(define (bytes-decode b)
  ;; (base64-decode b)
  (hex-bytes->bytes b)
  )

(define (make-auth-tag data atime tid iv)
  (let ()
    (sha1 (string->bytes/utf-8))))

(define (encrypt-data data key)
  ;; TODO: don't need to generate key, just iv.
  (let*-values ([(new-key iv) (generate-key cipher:aes-128)]
                [(in) (open-input-bytes data)]
                [(out) (open-output-bytes)]
                [(bytes) (encrypt cipher:aes-128 key iv in out)])
               (values (get-output-bytes out) iv )))

(define (decrypt-data data key iv)
  (let* ([in (open-input-bytes data)]
         [out (open-output-bytes)])
    (decrypt cipher:aes-128 key iv in out)
    (get-output-bytes out)))
   
;; Public

(define (scs-make-key)
  (match/values (generate-key cipher:aes-128)
    [(key _) key]))

(define (scs-encode data key)
  (let-values ([(edata iv) (encrypt-data data key)])
    (let* ([atime (current-seconds)]
           ;; TID ??
           [tid #"tid"])
      ;; Now encode them (base64)
      (let* ([enc-edata (bytes-encode edata)]
             [enc-atime (bytes-encode (string->bytes/utf-8 (number->string atime)))]
             [enc-tid (bytes-encode tid)]
             [enc-iv (bytes-encode iv)]
             [boxed (bytes-append enc-edata #"|" enc-atime #"|" enc-tid #"|" enc-iv)]
             [auth-tag (sha1 boxed)]
             [cookie (bytes-append boxed #"|" (bytes-encode auth-tag))])
        cookie))))

(define (scs-encode/key data)
  ;; TODO: don't need iv, just KEY!
  (let*-values ([(key _iv) (generate-key cipher:aes-128)]
                [(edata iv) (encrypt-data data key)])
    (let* ([atime (current-seconds)]
           ;; TID ??
           [tid #"tid"])
      ;; Now encode them (bas64)
      (let* ([enc-edata (bytes-encode edata)]
             [enc-atime (bytes-encode (string->bytes/utf-8 (number->string atime)))]
             [enc-tid (bytes-encode tid)]
             [enc-iv (bytes-encode iv)]
             [boxed (bytes-append enc-edata #"|" enc-atime #"|" enc-tid #"|" enc-iv)]
             [auth-tag (sha1 boxed)]
             [cookie (bytes-append boxed #"|" (bytes-encode auth-tag))])
        (values cookie key)))))
  
(define (scs-decode s key)
  (match (regexp-match #px#"^([^|]*)\\|([^|]*)\\|([^|]*)\\|([^|]*)\\|([^|]*)$" s)
    [(list _ enc-edata enc-atime enc-tid enc-iv enc-auth-tag)
     (let ([edata (bytes-decode enc-edata)]
           [atime (bytes-decode enc-atime)]
           [tid (bytes-decode enc-tid)]
           [iv (bytes-decode enc-iv)]
           [auth-tag (bytes-decode enc-auth-tag)]
           [calc-auth-tag (sha1 (bytes-append enc-edata #"|" enc-atime #"|" enc-tid #"|" enc-iv))])
       (cond
         [(not (bytes=? auth-tag calc-auth-tag))
          #f]
         [#t
          (values (decrypt-data edata key iv) (string->number (bytes->string/utf-8 atime)) tid iv)]))]))
         

(define (test s #:key (key #f))
  (let-values ([(cookie key) (if key 
                                 (scs-encode s key)
                                 (scs-encode/key s))])
    (scs-decode cookie key)))