#lang scheme

(define (factorial n)
  (if (null? n) 
      (factorial (car (cons n '())))
      (factorial (car (cons n '())))))
(factorial '(1))