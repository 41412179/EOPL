#lang racket

;Exercise 2.24
(define stack '())
(define empty-stack
  stack)
;(list? empty-stack)
(define (push ele stack)
  (cons ele stack))
(define (pop stack)
  (if (null? stack)
      "stack is empty!"
      (cdr stack)))
(define (top stack)
  (car stack))
(define (empty-stack? stack)
  (if (null? stack)
      #t
      #f))
