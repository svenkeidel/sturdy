#lang racket

;; Expected result: True

(begin
   (define (create-n n)
     (begin (define ($do-loop0 n a) (if (= n 0) a (begin ($do-loop0 (- n 1) (cons '() a))))) ($do-loop0 n '())))
   (define *ll* (create-n 200))
   (define (recursive-div2 l) (cond ((null? l) '()) (else (cons (car l) (recursive-div2 (cddr l))))))
   (define result (recursive-div2 *ll*))
   result

      )