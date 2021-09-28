;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Homework3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
code graveyard

(define flatten
  (lambda (slist)
    (cond
      ((list? (car slist))1)
      (else 2))))

|#

;1.27
(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((list? (car slist)) (append(flatten(car slist))(flatten(cdr slist))))
      ((symbol? (car slist)) (append (list(car slist)) (flatten (cdr slist )))))))


;(flatten '((a b) c () (d (e))))
             
;1.28
(define merge
  (lambda (l1 l2)
    (cond
      ((null? l1) (if (null? l2) '()  (cons (car l2) (merge l1 (cdr l2)))))
      ((null? l2) (cons (car l1) (merge (cdr l1) l2)))
      ((< (car l1) (car l2)) (cons (car l1)  (merge (cdr l1) l2)))
      ((< (car l2) (car l1)) (cons (car l2)  (merge  l1 (cdr l2))))
      (else (cons car l1  (merge (cdr l1) l2))))))

(merge '(4 5 6) '(1 2 3 55 66 77 88 989))
      
