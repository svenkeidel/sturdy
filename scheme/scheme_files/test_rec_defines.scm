(define (factorial n) (if (= n 1) 1 (* n (factorial (- n 1)))))
(define res (factorial 6))
res
