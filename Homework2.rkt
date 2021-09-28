;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Homework2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
(define product
  (lambda (sos1 sos2)
  (map (lambda (sos2)
      (if (null? sos2)
          '()
          (cons (cons(sos1 (car sos2))) (product(cdr sos1 cdr sos2))))))))

(product '(x y z) '(a b))



(define product
  (lambda (sos1 sos2)
    (cond
      ((not(null? sos2)) (cons(cons(car sos1) (list(car sos2))) (product sos1 (cdr sos2))))
      ((null? sos2) '())
      (else (cons(cons (car sos1) (list(car sos2))) (product sos1 (cdr sos2)))))))

(define product
  (lambda (sos1 sos2)
    (cond
      ((null? sos1) '())
      ((list? sos1) (cons((product (car sos1) sos2))
      ((not (list? sos1)) (cons(cons sos1 (list(car sos2))) (product sos1 (cdr sos2))))
      (else '()))))))


(define product
  (lambda (sos1 sos2)
    (cond
      ((null? sos1) '())
      ((null? sos2) '())
      (else
      
                   



(product '(x y z) '(a b))




 |#

;1.21
(define combine
  (lambda (x lst)
    (if(null? lst)
       '()
       (cons(cons x (list(car lst))) (combine x (cdr lst))))))

(combine 'x '(1 2))

(define product
  (lambda (sos1 sos2)
    (cond
      ((null? sos1) '())
      (else (append (combine (car sos1) sos2) (product (cdr sos1) sos2))))))


(product '(x y z) '(a b))

;1.22
(define filter-in
  (lambda ( pred lst)
    (cond
      ((null? lst) '())
      ;((list? (car lst)) (cons (filter-in pred (car (car lst))) (filter-in pred (cdr (car lst)))))
      ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)) ))
      (else (filter-in pred (cdr lst)))
      )))

    (filter-in  symbol? '(foo (a x) 1 2 fo 4))

;1.23
(define list-index
  (lambda (pred lst)
    (list-indexFinder pred lst 0)))

(define list-indexFinder
  (lambda (pred lst pos)
    (cond
      ((null? lst)'#f)
      ((pred (car lst)) pos)
      (else (list-indexFinder pred(cdr lst) (+ pos 1))))))

(list-index number? '(a 1 2(1 2)))
          
;1.24
(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst)) (every? pred (cdr lst)))
      (else #f))))
;1.25
 (define exists?
  (lambda (pred lst)
   (not (every? pred lst))))
                   


