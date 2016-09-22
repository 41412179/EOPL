#lang racket
;Contracts remove-first: List * SchemeVal -> List
(define (remove-first lst element)
  (if (null? lst)
      '()
      (cond ((equal? element (car lst)) (cdr lst))
            (else (cons (car lst) (remove-first (cdr lst) element))))))
;(remove-first '(1 2 3) 4)
;exercise
(define (remove-all lst element)
  (if (null? lst)
      '()
      (cond ((equal? element (car lst)) (remove-all (cdr lst) element))
            (else (cons (car lst) (remove-all (cdr lst) element))))))
(remove-all '(1 2 3 3) 3)