#lang scheme

(provide set-cdr! or)

(define-syntax or
  (syntax-rules ()
    [(or a) a]
    [(or a b ...) (if a #t (or b ...))]))

(define-syntax set-cdr!
  (syntax-rules ()
    ((set-cdr! location value)
     (set! location (cons (car location) value)))))