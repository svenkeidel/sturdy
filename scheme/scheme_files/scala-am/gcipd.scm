#lang scheme
;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: 36, passes
(letrec ((id (lambda (x) x))
         (f (lambda (n)
              (if (<= n 1)
                1
                (* n (f (- n 1))))))
         (g (lambda (n)
              (if (<= n 1)
                1
                (+ (* n n) (g (- n 1)))))))
  (+ ((id f) 3) ((id g) 4)))
