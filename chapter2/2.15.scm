(module our-2.15 (lib "eopl.ss" "eopl")
  (require "utils.scm")
  (define identifier? symbol?)
  (define-datatype program program?
    (a-program
     (exp1 expression?)))
  (define-datatype expression expression?
    (const-exp
     (num number?))
    (diff-exp
     (exp1 expression?)
     (exp2 expression?))
    (zero?-exp
     (exp1 expression?))
    (if-exp
     (exp1 expression?)
     (exp2 expression?)
     (exp3 expression?))
    (var-exp
     (var identifier?))
    (let-exp
     (var identifier?)
     (exp1 expression?)
     (body expression?)))
  (define empty-env
    (lambda()
      ('())))
  ;(empty-env)
  (define (extend-env var val env)
    (list (list var val) env))
    
  ;init env
  (define init-env
    (lambda ()
      (extend-env
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))
  (define run
    (lambda(string)
      (value-of-program (scan&parse string))))
  (define value-of-program
    (lambda(pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))
  (define (value-of exp env)
    (cases expression exp
      (const-exp (num)
                 (num-val exp))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval-num val1))
                        (num2 (expval-num val2)))
                    (num-val (- num1 num2)))))
      (zero?-exp (exp)
                 (let (num (value-of exp env))
                   (if (zero? num)
                       (bool-val #t)
                       (bool-val #f))))
      (if-exp (exp1 exp2 exp3)
              (let (num1 (val-bool (value-of exp1 env)))
                (if num1
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (var-exp (var)
               (apply env var))
      (let-exp (var exp1 exp2)
               (let ((val1 (value-of exp1 env)))
                 (value-of exp2 (extend-env var val1 env)))))))
  
      