#lang racket
;Exercise 1.15[*]
;Contracts: Int * SchemeVal -> List
;usage: get a list of Int times SchemeVal
(define our-duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (our-duple (- n 1) x)))))
(our-duple 2 3)
(our-duple 4 '(ha ha))
(our-duple 0 '(abcd))

;Exercise 1.16[*]
;Contracts: 2-List -> 2-List
(define our-2-list
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (reverse (car lst)) (our-2-list (cdr lst))))))
;reverse a 2-list
(define reverse
  (lambda (lst)
    (cons (cadr lst)
          (cons (car lst) '()))))
(our-2-list '((a 1) (a 2) (1 b) (2 b)))



;Exercise 1.17
;list -> list
(define (down lst)
  (if (null? lst)
      '()
      (cons (wrap (car lst)) (down (cdr lst)))))
(define (wrap lst)
  (list lst))
(down '((a) (fine) (idea)))
;contract: slist -> slist
(define (swapper s1 s2 slist)
  (cond ((null? slist) '())
        (else (cons (swapper-sexp s1 s2 (car slist))
                    (swapper s1 s2 (cdr slist))))))
;Exercise 
;s-exp -> 
(define (swapper-sexp s1 s2 lst)
  (if (symbol? lst)
      (if (eq? s1 lst)
          s2
          (if (eq? s2 lst)
              s1
              lst))
      (swapper s1 s2 lst)))
(swapper 'a 'd '(a b c d))
(swapper 'x 'y '((x) y (z (x))))

(define (list-set lst n x)
  (if (= n 0)
      (cons x lst)
      (cons (car lst)
            (list-set (cdr lst) (- n 1) x))))
(list-set '(a b c d) 2 '(1 2))

;Exercise 1.20
(define (count-occurrences s slist)
  (if (null? slist)
      0
      (+ (count-occur-sexp s (car slist))
         (count-occurrences s (cdr slist)))))
(define (count-occur-sexp s exp)
  (if (symbol? exp)
      (if (eq? exp s)
          1
          0)
      (count-occurrences s exp)))
(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))

;Exercise 1.21
(define (product sos1 sos2)
  (if (null? sos1)
      '()
      (cons (tmp (car sos1) sos2)
            (product (cdr sos1) sos2))))
(define tmp
  (lambda (s sos2)
    (if (null? sos2)
        '()
        (cons (list s (car sos2))
              (tmp s (cdr sos2))))))
(product '(a b c) '(x y))

(define (filter-in pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))
(filter-in number? '(a 2 (1 3) b 7))
(define (list-index pred lst)
  (list-index-tmp pred lst 0))
(define (list-index-tmp pred lst n)
  (if (null? lst)
      #f
      (if (pred (car lst))
          n
          (list-index-tmp pred (cdr lst) (+ n 1)))))
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(1 2 (a b) 3))

(define (every? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every? pred (cdr lst))
          #f)))
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))

(define (exists? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (exists? pred (cdr lst)))))
(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))
(define (up lst)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (if (not (null? (car lst)))
              (cons (caar lst)
                    (up (cons (cdar lst) (cdr lst))))
              (up (cdr lst)))
          (cons (car lst)
                (up (cdr lst))))))
(up '((1 2) (3 4)))

;you should know append's use
(define (flatten slist)
  (cond ((null? slist) '())
        ((not (pair? slist)) (list slist))
        (else (append (flatten (car slist))
                      (flatten (cdr slist))))))
(flatten '((a) () (b ()) () (c)))

(define (merge loi1 loi2)
  (cond ((null? loi1)
         loi2)
        (else (if (<= (car loi1) (car loi2))
                  (cons (car loi1)
                        (merge (cdr loi1) loi2))
                  (cons (car loi2)
                        (merge loi1 (cdr loi2)))))))
(merge '(1 4) '(1 2 8))

                     
      