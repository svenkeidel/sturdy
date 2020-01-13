#lang racket

(define (remove xs)
  (if (null? xs)
      '(1)
      (remove (cdr xs))))

(remove '(2 3 4))