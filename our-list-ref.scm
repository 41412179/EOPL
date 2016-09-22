#lang eopl
;our-list-ref: List * Int -> SchemeVal 
(define (our-list-ref lst n)
  (if (null? lst)
      (report-list-too-short n)
      (if (zero? n)
          (car lst)
          (our-list-ref (cdr lst) (- n 1)))))
(define (report-list-too-short n)
  (eopl:error 'our-list-ref
              "List too short by ~s elements.~%" (+ n 1)))
(our-list-ref '(1 '(2 3) 3 4) -1)
