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
    (case stack sk
      (empty-stack-record ()
                          (error "Empty stack!"))
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
                          (error "Empty stack!"))
      (push-record (e s) e)
      (pop-record (s) (top s))))
  (define (empty-stack? sk)
    (cases stack sk
      (empty-stack-record () #t)
      (push-record (e s) #f)
      (pop-record (s) (empty-stack? s))))
  (define e (empty-stack))
  (set! e (push 1 e))
  (set! e (push 2 e))
  (set! e (push 3 e))
  
  (equal?? (top e) 3)
  (define x (top e))
  (equal?? x 2)
  (set! e (pop e))
  ;(set! e (pop e))
  ;(empty-stack? e)
  
  )