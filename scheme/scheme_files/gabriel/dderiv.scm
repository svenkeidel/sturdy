#lang scheme

(define (equal? x y)
  (if (eq? x y)
      #t
      (if (and (null? x) (null? y))
          #t
          (if (and (cons? x) (cons? y))
              (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
              #f))))

(define (map f l)
  (if (null? l)
      l
      (if (pair? l)
          (cons (f (car l)) (map f (cdr l)))
          (error "Cannot map over a non-list"))))

(define (lookup key table)
  (letrec ((loop (lambda (x)
                   (if (null? x)
                       #f
                       (let ((pair (car x)))
                         (if (eq? (car pair) key)
                             pair
                             (loop (cdr x))))))))
    (loop table)))
(define properties '())
(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
              (cdr y)
              #f))
        #f)))
(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
              ;changed set-cdr
              (set! y (cons (car y) val))
              (set! x (cons (car x) (cons (cons key2 val) (cdr x))))))
        (set! properties
          (cons (cons key1 (cons (cons key2 val) '())) properties)))))
(define (dderiv a)
  (if (not (pair? a))
      (if (eq? a 'x) 1 0)
      (let ((f (get (car a) 'dderiv)))
        (if f
            (f a)
            (error "No derivation method available")))))

(define (my+dderiv a)
  (cons '+
        (map dderiv (cdr a))))

(define (my-dderiv a)
  (cons '-
        (map dderiv (cdr a))))

(define (*dderiv a)
  (cons '*
        (cons a
              (cons (cons '+
                          (map (lambda (a) (cons '/ (cons (dderiv a) (cons a '())))) (cdr a)))
                    '()))))

(define (/dderiv a)
  (cons '-
        (cons (cons '/
                    (cons (dderiv (cadr a))
                          (cons (caddr a) '())))
              (cons (cons '/
                          (cons (cadr a)
                                (cons (cons '*
                                            (cons (caddr a)
                                                  (cons (caddr a)
                                                        (cons (dderiv (caddr a))
                                                              '()))))
                                      '())))
                    '()))))
(put '+ 'dderiv my+dderiv)
(put '- 'dderiv my-dderiv)
(put '* 'dderiv *dderiv)
(put '/ 'dderiv /dderiv)
(let ((arg '(+ (* 3 x x) (* a x x) (* b x) 5))
      (result '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                  (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                  (* (* b x) (+ (/ 0 b) (/ 1 x)))
                  0)))
  (equal? (dderiv arg) result))
