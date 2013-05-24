#lang racket/base
;; For legal info, see file "info.rkt".

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/port
         racket/tcp)

(provide cgi
         get-request-header get-request-content-length get-request-input)

(define (invalid-cgi-type-error sym type)
  (error sym "Invalid CGI type: ~S" type))

(define (cgi-context-only-error sym)
  (error sym "Only works in CGI context"))

(define (cgi-request-context-only-error sym)
  (error sym "Only works in CGI request context"))


(define (bytes->number x)
  ;; TODO: Make this more efficient.
  (string->number (bytes->string/latin-1 x)))

(struct request
  (header
   content-length
   input-stream))

(define (read-scgi-header in)
  (begin0 
    (cond
      ;; TODO: Compare performance of "regexp-match" and "regexp-try-match".
      ;; Also compare performance to byte-by-byte I/O without regexps.
      [(regexp-match #rx"^[0-9]+" in)
       => (lambda (m)
            (or (eqv? 58 (read-byte in)) ; colon
                (error 'read-scgi-header "Expected colon after ~S." (car m)))
            ;; Note: We do the read non-tail-recursively, so that we don't
            ;; have to reverse the result.
            (make-immutable-hash 
             (let loop ([size-left (bytes->number (car m))]
                        [accum '()])
               (if (zero? size-left)
                   '()
                   (cond
                     ;; TODO: Should we limit this regexp as well?  Or rewrite
                     ;; it to do char-by-char I/O and not use regexp?
                     [(regexp-try-match #rx"^([^\000]+)\000([^\000]*)\000" in 0 size-left)
                      => (lambda (m)
                           (cons (cons (list-ref m 1)
                                       (list-ref m 2))
                                 (loop (- size-left (bytes-length (list-ref m 0)))
                                       (cons (cons (list-ref m 1) (list-ref m 2)) accum) )))]
                     [else
                      (error 'read-scgi-header "Could not read SCGI header with ~S bytes remaining of ~S." 
                             size-left (car m))])))))]
      [else (error 'read-scgi-header
                   "Did not read size number of SCGI header.")])
    (or (eqv? 44 (read-byte in))
        (error 'read-scgi-header "Could not read comma in SCGI header."))))

(define (write-response-header out header)
  (let loop ([header header])
    (match header
      [(list) #f]
      [(list-rest (cons name value) rest)
       (fprintf out "~A:~A" name value)
       (display "\r\n" out)
       (loop rest)]))
  (display "\r\n" out))

(define (write-response-body out body)
  ;; do it.

  (write-bytes body out))

(define (handle-scgi-accept in out proc request-id)
  (let* ([request-exit-status 0]
         [header (read-scgi-header in)]
         [content-length
            (bytes->number
             (or (hash-ref header #"CONTENT_LENGTH")
                 (error 'handle-scgi-accept
                        "~S missing CONTENT_LENGTH in ~S"
                        request-id
                        header)))]
         [content-in (make-limited-input-port in content-length)]
         [req (request header content-length content-in)])
    
    ;; Do somthing to protect from exceptions...
    (let-values ([(status message response-header response-body) (proc req)])
      ;; Write response line.
      (fprintf out "HTTP/1.1 ~A ~A" status message)
      
      ;; For now we do a simplistic bytes body.
      (let* ([body-bytes (cond
                           [(string? response-body)
                            (string->bytes/utf-8 response-body)]
                           [(bytes? response-body)
                            response-body])]
             [response-content-length (bytes-length body-bytes)]
             [header2 (cons `("Content-Length" . ,response-content-length) response-header)])
      
        ;; Write header.
        
        (write-response-header out header2)
      
        ;; Write body.
        (write-response-body out body-bytes)
      
        (flush-output out))
      )))

;; Public

(define (cgi #:startup             (startup-proc        void)
             #:request              request-proc
             #:shutdown            (shutdown-proc       void)
             #:scgi-hostname       (scgi-hostname       "127.0.0.1")
             #:scgi-max-allow-wait (scgi-max-allow-wait 4)
             #:scgi-portnum        (scgi-portnum        4000)
             #:reuse-scgi-port?    (reuse-scgi-port?    #t))
  (let ([type (if (getenv "REQUEST_URI") 'normal 'scgi)])
    (startup-proc)
    (dynamic-wind   
     (λ () #f)
     (λ ()
       (case type               
         [(scgi)
          (let ([listener-cust (make-custodian)])
            (parameterize ((current-custodian listener-cust))
              (let ([listener (tcp-listen scgi-portnum
                                          scgi-max-allow-wait
                                          reuse-scgi-port?
                                          scgi-hostname)])
                (dynamic-wind
                 (λ () #f)
                 (λ ()
                   (let loop ([request-id 1])
                     (let ([request-cust (make-custodian listener-cust)])
                       (parameterize ((current-custodian request-cust))
                         (let-values ([(in out) (tcp-accept/enable-break listener)])
                           (thread (lambda ()
                                     (dynamic-wind
                                      (λ () #f)
                                      (λ () (handle-scgi-accept in out request-proc request-id))
                                      (λ () (custodian-shutdown-all request-cust)))))))
                       (loop (+ 1 request-id)))))
                 (λ ()
                   (custodian-shutdown-all listener-cust)
                   (sleep 1))))))]
         ;;[(normal)
          ;; TODO: Set up end-cgi-request?
         ;; (parameterize ((cur-cgi-request-id 0))
          ;;  (request-proc))]
         [else (invalid-cgi-type-error 'cgi)]))
     (λ ()
       (parameterize-break #t
                           (shutdown-proc))))))

(define (get-request-header req)
  (request-header req))

(define (get-request-content-length req)
  (request-content-length req))

(define (get-request-input req)
  (request-input-stream req))



