
(module Excise2.22 (lib "eopl.ss" "eopl")
  (require "utils.scm")
  (define value?
    (lambda(v)
      #t))
  (define-datatype stack stack?
    (empty-stack-record);if stack's instance is '()
    (push-record (e value?)
                 (s stack?))
    (pop-record (s stack?)))
  
  (define (pop sk)
    (cases stack sk
      (empty-stack-record ()
                          (display "Empty stack!"))
      (push-record (e s) s)
      (pop-record (s) s)))
  (define empty-stack
    (lambda()
      (empty-stack-record)))
  (define (push e s)
    (push-record e s))
  (define (top sk)
    (cases stack sk
      (empty-stack-record ()
                          (display "Empty stack!"))
      (push-record (e s) e)
      (pop-record (s) (top s))))
  (define (empty-stack? sk)
    (cases stack sk
      (empty-stack-record () #t)
      (push-record (e s) #f)
      (pop-record (s) (empty-stack? s))))
  )