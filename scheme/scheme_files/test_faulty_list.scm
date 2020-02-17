#lang scheme

(define x 2)
(set! x 0)
(car (if (zero? x) '(#f) '(1 2)))