#lang eopl
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))
(define lst '((x 3) (y 4) (z 8)))

(define has-binding?
  (lambda (var a-list)
    (if (null? a-list)
        #f
        (if (eq? var (caar a-list))
            #t
            (has-binding? var (cdr a-list))))))
;test
(has-binding? 'y lst)
