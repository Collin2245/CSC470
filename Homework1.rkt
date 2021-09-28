;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define duple 
 (lambda (n x)
  (if (= n 0)
   '()
    (cons x (duple (- n 1) x)))))

(define invert
 (lambda (lst)
  (if (null? lst)
    '()
    (cons (cons (car (cdr (car lst))) (cons (car (car lst)) '())) (invert (cdr lst))))))

(define down
 (lambda (lst)
  (if (null? lst)
    '()
     (cons (list (car lst)) (down (cdr lst))))))