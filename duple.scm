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








