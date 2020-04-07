(define (append l x)
  (if (null? l)
      x
      (cons (car l) (append (cdr l) x))))

(define (one-to n)
  (letrec ((loop (lambda (i l)
                   (if (= i 0)
                       l
                       (loop (- i 1) (cons i l))))))
    (loop n '())))

(define (ok? row dist placed)
  (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

(define (try-it x y z)
  (if (null? x)
      (if (null? y)
          1
          0)
      (+ (if (ok? (car x) 1 z)
             (try-it (append (cdr x) y) '() (cons (car x) z))
             0)
         (try-it (cdr x) (cons (car x) y) z))))

(define (nqueens n)
  (try-it (one-to n) '() '()))

(nqueens 8)
