#lang racket
(define (subst new old lst)
  (cond ((null? lst) '())
        (else (cons (subst-in-s-exp new old (car lst))
              (subst new old (cdr lst))))))
(define (subst-in-s-exp new old s-exp)
  (cond ((symbol? s-exp)
         (if (eq? s-exp old)
             new
             s-exp))
        (else (subst new old s-exp))))
(subst 'a 'b '((b c) (b () d)))
