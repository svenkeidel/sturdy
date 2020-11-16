(define (remove xs)
  (if (null? xs)
      '()
      (remove (cdr xs))))

(remove '(2 3 4))
