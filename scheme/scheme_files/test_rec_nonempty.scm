(define (remove xs)
  (if (null? xs)
      (car '(1))
      (remove (cdr xs))))

(remove '(2 3 4))
