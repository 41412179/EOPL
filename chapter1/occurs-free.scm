#lang racket
(define occurs-free?
  (lambda (var exp)
    (cond ((not (symbol? var)) #f)
          (else (cond ((symbol? exp) (not (equal? var exp)))
                      ((equal? 'lambda (car exp))
                       (if (eq? var (car (cadr exp)))
                       #f
                       (occurs-free? var (cddr exp))))
                      (else (or (occurs-free? var (car exp))
                                (occurs-free? var (cdr exp)))))))))
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x 'y)
(occurs-free? 'x 'x)
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))