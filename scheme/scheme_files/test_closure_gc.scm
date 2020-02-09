#lang scheme 

(let ((double (lambda (x) (+ x x)))
      (and (square (y) (* y y)))
      (apply-fn (lambda (f n) (f n))))
  (apply-fn double 3)
  (apply-fn square 4))