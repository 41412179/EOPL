#lang racket
(define (subst new old lst)
  (cond ((null? lst) '())
        ((symbol? (car lst))
         (if (eq? old (car lst))
             (cons new (subst new old (cdr lst)))
             lst))
        (else (cons (subst new old (car lst)) (subst new old (cdr lst))))))
(subst 'a 'b '((b c) (b () d)))
