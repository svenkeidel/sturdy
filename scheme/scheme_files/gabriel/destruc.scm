(include-equals)

(define (length list)
  (begin
    (define (loop1 l n)
      (if (cons? l)
          (loop1 (cdr l) (+ n 1))
          (if (null? l)
              n
              (error "(length): contract violation, expected list"))))
    (loop1 list 0)))

(define (append-to-tail! x y)
  (if (null? x)
      y
      (begin
        (define (loop a b)
          (if (null? b)
              (begin
                (set-cdr! a y)
                x)
              (loop b (cdr b))))
        (loop x (cdr x)))))

(define (destructive n m)
  (let ((l (begin
             (define ($do-loop0 i a)
               (if (= i 0)
                   a
                   (begin
                     ($do-loop0 (- i 1) (cons '() a)))))
             ($do-loop0 10 '()))))
    (begin
      (define ($do-loop1 i)
        (if (= i 0)
            l
            (begin
              (cond
               ((null? (car l))
                (begin
                  (define ($do-loop2 l)
                    (if (null? l)
                        (begin)
                        (begin
                          (if (null? (car l))
                              (set-car! l (cons '() '())))
                          (append-to-tail! (car l)
                                           (begin
                                             (define ($do-loop3 j a)
                                               (if (= j 0)
                                                   a
                                                   (begin
                                                     ($do-loop3 (- j 1) (cons '() a)))))
                                             ($do-loop3 m '())))
                          ($do-loop2 (cdr l)))))
                  ($do-loop2 l)))
               (else
                (begin
                  (define ($do-loop4 l1 l2)
                    (if (null? l2)
                        (begin)
                        (begin
                          (define ($do-loop5 j a)
                            (if (zero? j)
                                a
                                (begin
                                  (set-car! a i)
                                  ($do-loop5 (- j 1) (cdr a)))))
                          (let ((ret ($do-loop5 (quotient (length (car l2)) 2) (car l2))))
                           (set-cdr!
                            ret
                            (let ((n (quotient (length (car l1)) 2)))
                              (cond
                               ((= n 0) (set-car! l1 '()) (car l1))
                               (else
                                (begin
                                  (define ($do-loop6 j a)
                                    (if (= j 1)
                                        (let ((x (cdr a)))
                                          (set-cdr! a '()) x)
                                        (begin
                                          (set-car! a i)
                                          ($do-loop6 (- j 1) (cdr a)))))
                                  ($do-loop6 n (car l1))))))))
                          ($do-loop4 (cdr l1) (cdr l2)))))
                  ($do-loop4 l (cdr l)))))
              ($do-loop1 (- i 1)))))
      ($do-loop1 n))))

(equal?
 (destructive 600 50)
 '((1 1 2)
   (1 1 1)
   (1 1 1 2)
   (1 1 1 1)
   (1 1 1 1 2)
   (1 1 1 1 2)
   (1 1 1 1 2)
   (1 1 1 1 2)
   (1 1 1 1 2)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3)))
