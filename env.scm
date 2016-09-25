#lang racket
(define empty-env
  (lambda ()
    '()))
(empty-env)
(define empty-env-v2
  (lambda ()
    (list '())))
(empty-env-v2)
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define apply-env
  (lambda (var env)
    (if (null? env)
        "var not exists"
        (if (eq? (car env) 'extend-env)
            (if (eq? (cadr env) var)
                (caddr env)
                (apply-env var (cadddr env)))
            "env error!"))))
(define env '(extend-env x 12 (extend-env y 14 ())))
(apply-env 'z env)
;Exercise 2.5
(define extend-env-v2
  (lambda (var val env)
    (list (cons var val) env)))
(define env-v2 '(extend-env-v2 x 12 (extend-env-v2 y 14 ())))
(extend-env-v2 'z 34 env-v2)
;Exercise 2.8
(define empty-env?
  (lambda (env)
    (if (null? env)
        #t
        #f)))
(empty-env? (empty-env))

;Exercise 2.9
(define has-binding?
  (lambda (s env)
    (if (not (empty-env? env))
        (if (eq? (caar env) s)
            #t
            (has-binding? s (cadr env)))
        #f)))
;debug
;(cdr env-test)
(define env-t (extend-env-v2 'y 23
                             (extend-env-v2 'z 34 (empty-env-v2))))
;test
(has-binding? 'z env-t)
;Exercise 2.10
(define has-binding-2-10
  (lambda (var env)
    (if (null? env)
(define env-2-10
  '((x . 23) ((y . 45) ())))
(caar env-2-10)
(define extend-env*
  (lambda (l-var l-val env)
    (if (null? l-var)
        env
        (if (has-binding? (car l-var) env)
            (extend-env* (cdr l-var) (cdr l-val) (update (car l-var) (car l-val) env))
            (extend-env* (cdr l-var) (cdr l-val) (cons (cons (car l-var) (car l-val)) env))))))
(define (update var val env)
  (if (eq? var (caar env))
      (cons (cons var val) (cdr env))
      (cons (car env) (update var val (cdr env)))))
(extend-env* '(z t) '(4 5) env-2-10)
