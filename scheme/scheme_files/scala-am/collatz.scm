#lang scheme
(require "../imports.scm")

(define (div2* n s)
  (if (= (* 2 n) s)
      n
      (if (= (+ (* 2 n) 1) s)
          n
          (div2* (- n 1) s))))

(define (div2 n)
  (div2* n n))

(define (hailstone* n count)
  (if (= n 1)
      count
      (if (even? n)
          (hailstone* (div2 n) (+ count 1))
          (hailstone* (+ (* 3 n) 1) (+ count 1)))))

(define (hailstone n)
  (hailstone* n 0))

(hailstone 5)
