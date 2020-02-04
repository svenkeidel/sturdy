#lang scheme

(define x 2)
(set! x 0)
(if (zero? x) '(1) '(1 2))