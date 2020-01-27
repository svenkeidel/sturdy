#lang scheme 

(let ((double (lambda (x) (+ x x)))
      (square (lambda (y) (and #f y)))
      (apply-fn (lambda (f n) (f n))))
  (apply-fn double 3)
  (apply-fn square #t))