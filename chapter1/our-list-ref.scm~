#lang racket
;our-list-ref List n -> nth element 
(define (our-list-ref lst n)
  (if (= n 0)
      (car lst)
      (if (empty? (cdr lst))
          "beyond bound"
          (our-list-ref (cdr lst) (- n 1)))))
(our-list-ref '(1 '(2 3) 3 4) -1)
