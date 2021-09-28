;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Homework4b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Write un-parse for our parsed no-exp that we wrote in class.  So that the following is true:
(No-unparser (no-parser ‘(call (function (x) x) a)))
Produces: (call (function (x) x) a)
Submit the code plus the self assessment



(define MyLanguage-unparser
  (lambda(parsedMyLanguage)
    (cond
      ((eq? (length parsedMyLanguage) 3) (append '(call) (list(MyLanguage-unparser (cdr parsedMyLanguage)))))
      ((eq? (length parsedMyLanguage) 2) (cons 'middleLine  (list(MyLanguage-unparser (cdr parsedMyLanguage)))))
      ((eq? (length parsedMyLanguage) 1) (resolve 'var-exp (list parsedMyLanguage)))
      (else 0))))


|#
(define env '((age 21)))

(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(resolve 'age env)




(define MyLanguage-parser
  (lambda (MyLanguage-code)
    (cond
      ((symbol? MyLanguage-code) (list 'var-exp MyLanguage-code))
      ((eq? (car MyLanguage-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr MyLanguage-code))
             (list 'body
                   (MyLanguage-parser (caddr MyLanguage-code)))))
      (else (list 'call-exp
                  (MyLanguage-parser (cadr MyLanguage-code))
                  (MyLanguage-parser (caddr MyLanguage-code)))))))



(define sample-MyLanguage-code '(call (function (x) x) a))
(MyLanguage-parser sample-MyLanguage-code)



(define MyLanguage-unparser
  (lambda (MyLanguage-code)
    (cond
      ((eq? (car MyLanguage-code) 'var-exp)
       (cadr MyLanguage-code))
      ((symbol? (cdadr MyLanguage-code)) (cdadr MyLanguage-code))
      ((eq? (car MyLanguage-code) 'func-exp)
       (list 'function (cdadr MyLanguage-code) (car (cdadr (caddr MyLanguage-code)))))
      (else (list 'call
                  (MyLanguage-unparser (cadr MyLanguage-code))
                  (MyLanguage-unparser (caddr MyLanguage-code)))))))



(display(MyLanguage-unparser (MyLanguage-parser sample-MyLanguage-code)))



