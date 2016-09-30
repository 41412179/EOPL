#lang eopl
;our-list-ref: List * Int -> SchemeVal
(define (our-list-ref-v2 lst n)
  (if (< n 0)
      (report-list-too-short lst n)
      (our-list-ref lst n)))
(define (our-list-ref lst n)
  (if (zero? n)
          (if (null? lst)
              (report-number-too-big n)
              (car lst))
          (our-list-ref (cdr lst) (- n 1))))
(define (report-list-too-short lst n)
  (eopl:error 'our-list-ref-v2
              "List ~s don't hold ~s th elements.~%" lst n ))
(define (report-number-too-big n)
  (eopl:error 'our-list-ref
              "List don't have enough elements~%"))
(our-list-ref-v2 '(1 '(2 3) 3 4) 1)
