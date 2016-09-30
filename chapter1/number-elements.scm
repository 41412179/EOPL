#lang racket
(define (number-elements lst n)
  (cond ((null? lst) '())
        (else (cons (list n (car lst))
                    (number-elements (cdr lst) (+ n 1))))))
(number-elements '(1 2 3) 3)
