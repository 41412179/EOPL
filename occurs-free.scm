#lang racket
(define occurs-free?
  (lambda (var exp)
    (cond ((not (symbol? var)) #f)
          (else (cond ((symbol? exp) (not (equal? var exp)))
                      ((equal? 'lambda (car exp))
                       (if (eq? (cadr exp) (cons var '()))
                       #f
                       (occurs-free? var (cddr exp))))
                      (else (or (occurs-free? var (car exp))
                                (occurs-free? var (cdr exp)))))))))
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x 'y)
(occurs-free? 'x 'x)
