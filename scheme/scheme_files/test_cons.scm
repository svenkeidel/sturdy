#lang scheme 

(define (equal? x y)
  (if (eq? x y)
      #t
      (if (and (null? x) (null? y))
          #t
          (if (and (cons? x) (cons? y))
              (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
              #f))))

(equal? (cdr (cdr '(2 3))) (cdr (cdr '(3 4))))
