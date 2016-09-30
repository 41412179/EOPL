#lang racket
;Contracts remove-first: List * SchemeVal -> List
(define (remove-first lst element)
  (if (null? lst)
      '()
      (cond ((equal? element (car lst)) (cdr lst))
            (else (cons (car lst) (remove-first (cdr lst) element))))))
(remove-first '(1 2 3) 4)
