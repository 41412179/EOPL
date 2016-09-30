#lang racket
(define list-length
  (lambda (list)
    (cond ((null? list) 0)
          (else (+ 1 (list-length (cdr list)))))))
(list-length '(1 2))
