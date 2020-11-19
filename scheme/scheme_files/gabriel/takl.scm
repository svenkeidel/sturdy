(include-equals)

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define (shorterp a b)
  (and (not (null? b))
       (or (null? a)
           (shorterp (cdr a)
                     (cdr b)))))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(let ((result '(7 6 5 4 3 2 1)))
  (equal? (mas (listn 18) (listn 12) (listn 6)) result))
