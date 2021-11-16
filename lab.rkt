;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;
; (cond
 ;     ((eq? (car block-exp) 'block-exp)
  ;     (+ 0 (count-block-exp (cdr block-exp))))
   ;   ((eq? (car block-exp) '())
    ;   (0))
     ; ((eq? (caar block-exp) 'number)
      ; (cadar block-exp))
      ;(else
       ;(+ 0 ))))) 
;1
(define block-exp-code '(block-exp (number 1)(symbol +)(symbol -)(number 2)(number 3)(number 4)(number 5)(number 6)(number 7)(number 8)))
(cdr block-exp-code)
;2
(define count-block-exp
  (lambda (block-exp)
 (cond
      ((eq?  block-exp '())
       0)
      ((eq? (car block-exp) 'block-exp)
       (+ 0 (count-block-exp (cdr block-exp))))
      ((list? (car block-exp))
       (+ (count-block-exp (car block-exp))  (count-block-exp (cdr block-exp))))
      ((eq? (car block-exp) 'number)
       (cadr block-exp))
      (else
       0 ))))
;3
(define collect-symbols
  (lambda (block-exp)
    (append (collect-symbols-helper block-exp))))

(define collect-symbols-helper
  (lambda (block-exp)
   (cond
    ((eq? block-exp '())
      '())
    ((eq? (car block-exp) 'block-exp)
       (collect-symbols-helper (cdr block-exp)))
      ((list? (car block-exp))
       (cons (collect-symbols-helper (car block-exp))  (collect-symbols-helper (cdr block-exp))))
      ((eq? (car block-exp) 'symbol)
       (cadr block-exp))
       (else
        null))))
      
     
   

(car block-exp-code)
;(collect-symbols block-exp-code)
(count-block-exp block-exp-code)
(collect-symbols block-exp-code)

