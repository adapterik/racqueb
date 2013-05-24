#lang racket	

(require "mustachio2.rkt" "json2.rkt")


;;; ------------------------
;;; Testing 
;;; ------------------------


(define (say what)
  (display what))

(define (test1)
  (let ((template "Hi {{name}}, it is a {{weather}} day."))
    (let ((parsed (parse/stream (open-input-string template))))
      parsed)))

(define (test2)
  (let ((template "Hi {{name:upper:lower:lower:last}}, you were born on {{birtday}}, and you are {{age}} years old. Your pets are {{#pets}}{{name}} ({{type}}), {{/pets}}."))
    (let ((parsed (parse/stream (open-input-string template))))
      parsed)))

(define (test3)
  (let ((template "Hi {{name:upper:lower:lower:last}}, you were born on {{birtday}}, you are {{?male}}a boy{{/male}}{{?girl}}a girl{{/girl}}, and you are {{age}} years old. Your pets are {{#pets}}{{.}},{{name}} ({{type}}), {{/pets}}."))
    (let ((parsed (parse/stream (open-input-string template))))
      (render parsed '()))))


(define (test4)
  (let ((template "Hi {{name:upper:lower:lower:last}}, you were born on {{birthday}}, you are {{?male}}a boy{{/male}}{{?girl}}a girl{{/girl}}, and you are {{age}} years old. Your pets are {{#pets}}{{_this}}, {{name}} ({{type}}), {{/pets}}.")
        (db (string->jsexpr "{\"name\":\"Erik\", \"birthday\":\"the day after xmas\", \"male\": true, \"age\":50, \"pets\": [\"coco\", \"peet\", \"schnee\"]}")))
    (let ((parsed (parse/stream (open-input-string template))))
      (render parsed db))))
  
(define (test5)
  (let ((template "Hi {{name:upper:lower:lower:last}}, you were born on {{birthday}}, you are {{?male}}a boy{{/male}}{{?girl}}a girl{{/girl}}, and you are {{age}} years old. Your pets are {{#pets}}{{name}} ({{type}}), {{/pets}}."))
    (let ((parsed (parse/stream (open-input-string template))))
      parsed)))

(define (test6 start limit)
  (letrec ((r (lambda (n)
                (let* ((j (string->jsexpr (format "{\"mykey_~a\": \"my value\"}" n))))
                  (and (< n limit)
                       (r (+ 1 n)))))))
    (r start)))


(define (test7 start limit)
  (letrec ((r (lambda (n)
                (let* ((d (random 1000000000))
                       (s (format "{\"mykey_~a\": \"my value ~a\"}" d d))
                       (j (string->jsexpr s)))
                  (and (< n limit)
                       (r (+ 1 n)))))))
    (r start)))
              


(define (test8 iters)
  (letrec ((r (lambda (n)
                (and (< n iters)
                     (let ((template "Hi {{name:upper:lower:lower:last}}, you were born on {{birthday}}, you are {{?male}}a boy{{/male}}{{?girl}}a girl{{/girl}}, and you are {{age}} years old. Your pets are {{#pets}}{{_this}}, {{name}} ({{type}}), {{/pets}}.")
                           (db (string->jsexpr "{\"name\":\"Erik\", \"birthday\":\"the day after xmas\", \"male\": true, \"age\":50, \"pets\": [\"coco\", \"peet\", \"schnee\"]}")))
                       (let ((parsed (parse/stream (open-input-string template))))
                         (render parsed db))
                       (r (+ n 1)))))))
    (r 0)))

(define (test9)
  (string->jsexpr "{\"name\":\"Erik\", \"birthday\":\"the day after xmas\", \"male\": true, \"age\":50, \"pets\": [\"coco\", \"peet\", \"schnee\"]}"))

(define (test10 test-iters parse-iters)
  (letrec ((r (λ (n acc)
                (if (> n test-iters) acc
                    (let-values ([(result cpu real gc) (time-apply test8 `(,parse-iters))])
                      (r (+ n 1) (cons (list cpu real gc) acc)))))))
    (let* ((result (r 0 '()))
           (summed (foldl (λ (lst rslt)
                            (list (+ (list-ref rslt 0) (list-ref lst 0))
                                  (+ (list-ref rslt 1) (list-ref lst 1))
                                  (+ (list-ref rslt 2) (list-ref lst 2)))) '(0 0 0) result))
           (average (map (λ (x)
                           (/ x (length result))) summed)))
      (values summed average))))