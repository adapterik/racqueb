#lang racket

(require srfi/19)

(provide max-age->expires)

;; From racket web server
; seconds->gmt-string : Nat -> String
; format is rfc1123 compliant according to rfc2068 (http/1.1)
(define (seconds->gmt-string s)
  (let* ([local-date (seconds->date s)]
         [date (seconds->date (- s (date-time-zone-offset local-date)))])
    (format "~a, ~a ~a ~a ~a:~a:~a GMT"
            (vector-ref DAYS (date-week-day date))
            (two-digits (date-day date))
            (vector-ref MONTHS (sub1 (date-month date)))
            (date-year date)
            (two-digits (date-hour date))
            (two-digits (date-minute date))
            (two-digits (date-second date)))))

; two-digits : num -> str
(define (two-digits n)
  (let ([str (number->string n)])
    (if (< n 10) (string-append "0" str) str)))

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (max-age->expires max-age)
  (seconds->gmt-string (+ max-age (time-second (current-time)))))