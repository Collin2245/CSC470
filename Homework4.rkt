;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Homework4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
(merge '(4 5 6) '(1 2 3 55 66 77 88 989))


(merge '(3 4 99) '( 1 5) <)
(merge '(99 4 3) '(5 1) >)
|#



(define merge
  (lambda (l1 l2 pred)
    (cond
      ((null? l1) (if (null? l2) '()  (cons (car l2) (merge l1 (cdr l2) pred))))
      ((null? l2) (cons (car l1) (merge (cdr l1) l2 pred)))
      ((pred (car l1) (car l2)) (cons (car l1)  (merge (cdr l1) l2 pred)))
      ((pred (car l2) (car l1)) (cons (car l2)  (merge  l1 (cdr l2 ) pred)))
      (else (cons (car l1)  (merge (cdr l1) l2 pred))))))



(define mergeSort
  (lambda (lst pred)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) lst)
      (else (merge (list(car lst)) (mergeSort(cdr lst) pred) pred))
      )))

(mergeSort '(4 5 1 9 5  99 99 99 99 99 999) <)
(mergeSort '() <)


(define merge2
  (lambda (lst)
    (mergeSort lst <)))

(merge2 '(3 4 5 6))



