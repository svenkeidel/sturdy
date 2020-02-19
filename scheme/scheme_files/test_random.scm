#lang scheme
;; Expected result #t, passes
(define (extended-gcd a b)
  (if (= b 0)
      (cons 0 1)
      (let* ((x:y (extended-gcd a 0))
             (x (car x:y))
             (y (cdr x:y)))
        (cons y 1 ))))
(car (extended-gcd 0 1))