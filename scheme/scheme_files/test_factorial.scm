#lang scheme

(define (fac n) 
    (if (= n 1) 
        1
        (* n (fac (- n 1)))))

(fac 10)