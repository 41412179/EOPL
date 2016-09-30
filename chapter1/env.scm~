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

