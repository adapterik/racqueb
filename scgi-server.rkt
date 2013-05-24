#lang racket

(require net/url xml net/uri-codec racket/dict 
         "scgi2.rkt"  "utils.rkt" "scs.rkt"
         "coco-page.rkt" "coco-rest.rkt" "coco-help.rkt" "coco-debug.rkt" "coco-test.rkt"
         "coco-view.rkt" "coco-crud.rkt" "coco-auth.rkt" "coco-session.rkt")

(provide start-server)

(define (start-server port config)
  (let ([auth-key (scs-make-key)])
    (cgi 
     #:scgi-portnum port
     #:startup (λ ()
                 (printf "Starting up cgi"))
     #:request (λ (req)
                 (handle-connection req (cons `(scs-key . ,auth-key) config)))
     #:shutdown (λ ()
                  (printf "Shutting down.")))))

;;(define cgi-dfw-vhost (make-cgi-variable-proc 'cgi-dfw-vhost #"DFW_VHOST"))
;;(define cgi-dfw-host (make-cgi-variable-proc 'cgi-dfw-host #"DFW_HOST"))
;;(define cgi-dfw-module (make-cgi-variable-proc 'cgi-dfw-module #"DFW_MODULE"))
  
(define (handle-connection req config)
  (let* ([info (grok-request req config)]
         [next (get-dispatcher (car (dict-ref info 'path)))])
    (if next
        (let-values ([(status message header body) (next req info (cdr (dict-ref info 'path)))])
          (values 200 "OK" header body))
        (values 404 "Not found" '() (format "Sorry, this document was not found: ~A." (hash-ref info 'path))))))

;; The beginnings of coco

(define (get-dispatcher id)
  (let ([dispatchers (hash #"debug" do-debug
                           #"test" do-test
                           #"help" do-help
                           #"page" do-page
                           #"view" do-view
                           #"crud" do-crud
                           #"auth" do-auth
                           #"rest" do-rest)])
    (hash-ref dispatchers id #f)))


(define (grok-request req config)
  (let* ([header (get-request-header req)]
         [script-prefix (hash-ref header #"DFW_PREFIX")]
         [script-url (hash-ref header #"SCRIPT_URL")]
         [urlstring (subbytes script-url (bytes-length script-prefix))]
         [path (regexp-split #rx"/" urlstring)]
         [query-string (hash-ref header #"QUERY_STRING")]
         [query (form-urlencoded->alist (bytes->string/utf-8 query-string))]
         [vhost (string->symbol (bytes->string/utf-8 (hash-ref header #"DFW_VHOST")))]
         [session (get-session (hash-ref header #"HTTP_COOKIE" #"") (dict-ref config 'scs-key))])
    ;; (printf "Session: ~S~n" session)
    (hasheq 'path path 'query query 'config config
            'input (get-request-input req)
            'session session
            'method (string->symbol 
                     (string-downcase
                      (bytes->string/utf-8 
                       (hash-ref header #"REQUEST_METHOD"))))
            'vhost-id (hash-ref header #"DFW_VHOST")
            'vhost (dict-path config `(vhosts ,vhost)))))




;; The coco dispatcher
(define (coco-dispatcher)
  ;; Get the current path;
  ;; Break it apart into path components
  ;; Start matching
  #f
  )

;; The coco template server

(define (coco-template-server)
  #f
  )

;; The coco rest server
(define (coco-rest-server)
  #f
  )