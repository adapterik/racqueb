#lang racket

(require racket/dict
         "json2.rkt" 
         "utils.rkt")

(provide parse/stream parse/string parse/filename render)

;; -------------------------------------
;; Parsing
;; -------------------------------------

(define re-tag-start #px#"(?:([\\s\\S]*?)\\{\\{)|(?:([\\s\\S]*?)$)")

(define re-parseon #px#"(?:(.*?)(?:\\{\\{%[\\s]*parseon[\\s]*\\}\\}))|(?:([\\s\\S]*?)$)")

;; This one parses the tag into from the beginning of the tag to the first }}.
(define re-tag #px#"[ ]*([#^\\?\\/,\\@\\>%]{0,1})[ ]*([^ :\\}]+)[ ]*(.*?)\\}\\}")

(define re-modifier #px#"[ ]*\\:{1}?[ ]*([^ \\:]{1,})")

;; Core first pass parser.
(define (stream-parser s accum)
  (match (regexp-match re-tag-start s)
    [#f
     (reverse accum)]
    [(list #f #f #f) 
     (reverse accum)]
    [(list matched text #f)
     (match (parse-tag s)
       [(list 'pragma "parseoff")
        ;; Just parse up to the next occurence of {{%parseon}}
        (stream-parser/parseoff s (cons (list 'text (bytes->string/utf-8 text)) accum))]
       [result
        (stream-parser s (cons result (cons (list 'text (bytes->string/utf-8 text)) accum)))])]
    [(list m #f text)
     (reverse (cons (list 'text (bytes->string/utf-8 text)) accum))]))

(define (stream-parser/parseoff s accum)
  (match (regexp-match re-parseon s)
    [#f
     ;; Do we need this?
     (reverse accum)]
    [(list _ unparsed #f)
     (stream-parser s (cons (list 'text (bytes->string/utf-8 unparsed)) accum))]
    [(list _ #f unparsed)
     (stream-parser s (cons (list 'text (bytes->string/utf-8 unparsed)) accum))]))


;; Parsers for bits and pieces.

(define (parse-modifiers s)
  (if s
      (letrec ([p (lambda (s accum)
                    (match (regexp-match re-modifier s)
                      [#f (reverse accum)]
                      [(list m modifier)
                       (p s (cons modifier accum))]))])
        (p (open-input-bytes s) '()))                 
      '()))

(define (parse-name name)
  (map (λ (x) (string->symbol (bytes->string/utf-8 x))) (regexp-split #px"\\." name)))
  
(define (parse-tag s)
  (match (regexp-match re-tag s)
    [(list _ #"" property-name modifiers)
     (list 'property (parse-name property-name) (parse-modifiers modifiers))]
    [(list _ #">" include #"")
     (list 'include (bytes->string/utf-8 include))]
    [(list _ #"#" section-name #"")
     (list 'section (bytes->string/utf-8 section-name))]
    [(list _ #"/" end-name #"")
     (list 'end (bytes->string/utf-8 end-name))]
    [(list _ #"?" object-name #"")
     (list 'boolean (bytes->string/utf-8 object-name))]
    [(list _ #"^" object-name #"")
     (list 'inverse (bytes->string/utf-8 object-name))]
    [(list _ #"%" pragma-name #"")
     (list 'pragma (bytes->string/utf-8 pragma-name))]
    ))

;; The passthrough parser ignores everything up to the next
;; occurance of the supplied text which probably has the form
;; of {{~TAG}}.
  


;; Public api

(define (parse/stream s paths named-templates)
  (let ((parsed (stream-parser s '())))
    (analyze-parse parsed paths named-templates)))

;; get dir for file,
;; prefix to paths,
;; open file input stream
;; parse
;; analyze

;; Top level parsers

(define (parse/file filename paths named-templates)
  (call-with-input-file* filename
    (λ (s)
      (let ([parsed (stream-parser s '())])
        (analyze-parse parsed paths named-templates)))))

(define (parse/filename filename paths template-aliases)
  ; Then try to find it in a file.
  (let ([file (find-file-in-paths (or (dict-ref template-aliases filename #f) filename) paths)])
    (and file
         (call-with-input-file* file
           (λ (s)
             (let ([parsed (stream-parser s '())])
               (match-let-values ([(base _ _) (split-path file)])
                                 (analyze-parse parsed (cons base paths) template-aliases))))))))
   
(define (parse/string s named-templates)
  (let ([parsed (stream-parser (open-input-string s) '())])
    (analyze-parse parsed '() named-templates)))



(define (find-file-in-paths template-path paths)
  (ormap (λ (dir)
           (let ([path (build-path dir template-path)])
             (and (file-exists? path) path))) paths))

;; Pass 2 parser
(define (analyze-parse parsed paths named-templates)
  (letrec ((r (lambda (p accum section-end)
                (match p
                  [(list) (reverse accum)]
                  ;; TEXT
                  ;; Just copy the text.
                  [(list-rest (list 'text text) rest)
                   (r rest (cons (list 'text text) accum) section-end)]
                  ;; PROPERTY
                  ;; Just replicate the property node
                  [(list-rest (list 'property name modifiers) rest)
                   (r rest (cons (list 'property name modifiers) accum) section-end)]
                  ;; INCLUDE
                  ;; For each included item find, parse, and include the template, splicing it 
                  ;; into our accum list.
                  [(list-rest (list 'include path) rest)
                   (r rest (cons (list 'include path (parse/filename path paths named-templates)) accum) section-end)]
                  ;; BOOLEAN
                  ;; If the named property-name is found, just continue along. Similar to section,
                  ;; but we don't change the db context.
                  [(list-rest (list 'boolean name) rest)
                   (let-values ([(children rest) (r rest '() name)])
                     (r rest (cons (list 'boolean name children) accum) name))]
                  ;; INVERSE
                  ;; Like a boolean NOT.
                  [(list-rest (list 'inverse name) rest)
                   (let-values ([(children rest) (r rest '() name)])
                     (r rest (cons (list 'inverse name children) accum) name))]
                  ;; SECTION
                  ;; If the property is found, we take different iteration actions depending on what
                  ;; we have found. 
                  [(list-rest (list 'section name) rest)
                   (let-values ([(children rest) (r rest '() name)])
                     (r rest (cons (list 'section name children) accum) name))]
                  ;; PASSTHROUGH
                  ;; Just echo the text, do not parse at all.
                  
                  ;; END
                  ;; Ends a section, boolean, or inverse.
                  [(list-rest (list 'end name) rest)
                   (if (equal? name section-end)
                       (values (reverse accum) rest)
                       (error 'end-tag "Incorrect end tag ~s for starting tag ~s" name section-end))]))))
      (r parsed '() #f)))

;;; -----------------------------------
;;; Rendering
;;; -----------------------------------

(define (apply-modifiers value modifiers)
  value)

(define (get-property prop db [default 'undefined])
  ;;(printf "[get-property] Looking for ~S in ~S~n" prop db)
  (define (find-it name db)
    (cond
      [(hash? db)
       (if (hash-has-key? db name)
           (values #t (hash-ref db name))
           (values #f #f))]
      [(list? db)
       (let ([result (list-ref db name)])
         (if result (values #t result) (values #f #f)))]
      [(and (string? db) (eq? name '_this))
       (values #t db)]
      [else (values #f #f)]))
  (match prop
    [(list) db]
    [(list-rest name rest) 
     (let-values ([(found value) (find-it name db)])
       (if found (get-property rest value default) default))]))
    

(define (is-property? prop db)
  (eq? (get-property prop db) 'undefined))

(define (render out template db)
  (letrec ([rnd (lambda (tmp db)
                  (match tmp
                    ;; Terminal condition...
                    [(list) #t]
                    ;; TEXT
                    [(list-rest (list 'text text) rest)
                     (display text out)
                     (rnd rest db)]
                    ;; PROPERTY
                    ;; Just output the property-name value as looked up.
                    [(list-rest (list 'property name modifiers) rest)
                     (let ((value (apply-modifiers (get-property name db "") modifiers)))
                       (display value out)
                       (rnd rest db))]
                    ;; INCLUDE
                    ;; 
                    [(list-rest (list 'include path include-template) rest)
                     (rnd include-template db)
                     (rnd rest db)]

                    ;; BOOLEAN
                    ;; If the named property is found, just continue along. Similar to section,
                    ;; but we don't change the db context.
                    [(list-rest (list 'boolean name sub-template) rest)
                     (and (is-property? name db)
                          (rnd sub-template db))
                     (rnd rest db)]
                    
                    ;; INVERSE
                    ;; Like a boolean NOT.
                    [(list-rest (list 'inverse name sub-template) rest)
                     (and (not (is-property? name db))
                          (rnd sub-template db))
                     (rnd rest db)]
                    
                    ;; SECTION
                    ;; If the property is found, we take different iteration actions depending on what
                    ;; we have found. 
                    [(list-rest (list 'section name sub-template) rest)
                     (let ((next-db (get-property name db)))
                       (cond
                         [(eq? next-db 'undefined)
                          ;; Ignore it.
                          #f]
                         [(list? next-db)
                          (for-each (lambda (item-db)
                                      (rnd sub-template item-db)) next-db)]
                         [(hash? next-db)
                          (hash-for-each next-db (lambda (key value)
                                                   (rnd sub-template value)))]
                         [else
                          (display "SECTION NOT DEFINED FOR STRING, NUMBER, or BOOLEAN" out)])
                       (rnd rest db))]
                    
                    ))])
    (rnd template db)))



                   