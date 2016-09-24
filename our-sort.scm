#lang racket
;insert-sort
(define (insert-v2 lst res)
  (if (null? res)
      (if (not (null? lst))
          (insert-v2 (cdr lst) (cons (car lst) res))
          '())
      (if (null? lst)
          res
          (insert-v2 (cdr lst) (insert-one (car lst) res)))))
(define (insert-one ele res)
  (if (null? res)
      (cons ele res)
      (if (< ele (car res))
          (cons ele res)
          (cons (car res)
                (insert-one ele (cdr res))))))
(define (insert-v3 lst)
  (insert-v2 lst '()))
(insert-v3 '(8 2 5 2 3))

;returns a list of elements sorted
;by the predicate
(define (insert-v2-pred pred lst res)
  (if (null? res)
      (if (not (null? lst))
          (insert-v2-pred pred (cdr lst) (cons (car lst) res))
          '())
      (if (null? lst)
          res
          (insert-v2-pred pred (cdr lst) (insert-one-pred pred (car lst) res)))))
(define (insert-one-pred pred ele res)
  (if (null? res)
      (cons ele res)
      (if (pred ele (car res))
          (cons ele res)
          (cons (car res)
                (insert-one-pred pred ele (cdr res))))))
(define (insert-v3-pred pred lst)
  (insert-v2-pred pred lst '()))

(insert-v3-pred > '(8 2 5 2 3))


(define (leaf? lst)
  (and (symbol? (car lst))
       (number? (cadr lst))
       (number? (caddr lst))))
(leaf? '(foo 1 2))
(define (lson lst)
  (cadr lst))
(lson '(foo 1 2))
(lson '(foo (zoo 3 4) 5))
(define (rson lst)
  (caddr lst))
(rson '(foo (zoo 3 4) (boo 5 6)))
(rson '(foo 2 3))

(define (double-tree lst)
  (cond ((null? lst) '())
        ((list? lst)
         (cons (car lst) (list (double-tree (lson lst)) (double-tree (rson lst)))))
        (else (* lst 2))))
(double-tree '(foo 1 2))
(double-tree '(foo (zoo 3 4) (bar 4 5)))

(define (r-lson lst)
  (caddr lst))
(define (r-rson lst)
  (cadddr lst))
(define (head lst)
  (cadr lst))

(define (mark-leaves-with-red-depth lst n)
  (if (empty? lst)
      '()
      (cond ((eq? (car lst) 'interior-node)
             (cond ((eq? (head lst) 'red)
                    (cons 'red (list (mark-leaves-with-red-depth (r-lson lst) (+ n 1))
                                     (mark-leaves-with-red-depth (r-rson lst) (+ n 1)))))
                   (else (cons (head lst) (list (mark-leaves-with-red-depth (r-lson lst) n)
                                                (mark-leaves-with-red-depth (r-rson lst) n))))))
            (else n))))
(define (m-l-w-r-d lst n)
  (if (null? lst)
      '()
      (if (eq? (car lst) 'interior-node)
          (if (eq? (head lst) 'red)
              (list 'red
                    (m-l-w-r-d (r-lson lst) (+ n 1))
                    (m-l-w-r-d (r-rson lst) (+ n 1)))
              (list (head lst)
                    (m-l-w-r-d (r-lson lst) n)
                    (m-l-w-r-d (r-rson lst) n)))
          n)))

      
              
;test data
(mark-leaves-with-red-depth
'(interior-node 'red
               (interior-node 'bar
                              (leaf 26)
                              (leaf 12))
               (interior-node 'red
                              (leaf 11)
                              (interior-node 'quux
                                             (leaf 117)
                                             (leaf 14)))) 0)
(m-l-w-r-d
'(interior-node red
               (interior-node bar
                              (leaf 26)
                              (leaf 12))
               (interior-node red
                              (leaf 11)
                              (interior-node quux
                                             (leaf 117)
                                             (leaf 14)))) 0)
(m-l-w-r-d '() 0)
(m-l-w-r-d
'(interior-node 'red
                (leaf 34)
                (leaf 45))
 0)
(define test '(interior-node red
                (leaf 34)
                (leaf 45)))
;some debug data
;(r-lson test)
;(r-rson test)
;(head test)
;(eq? (head test) 'red)

