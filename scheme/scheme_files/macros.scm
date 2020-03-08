(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or a) a]
    [(or a b ...) (if a #t (or b ...))]))

(define-syntax set-cdr!
  (syntax-rules ()
    ((set-cdr! location value)
     (set! location (cons (car location) value)))))

(define-syntax include-equals
  (syntax-rules ()
    ((include-equals)
     (define (equal? x y)
       (if (eq? x y)
           #t
           (if (and (null? x) (null? y))
               #t
               (if (and (cons? x) (cons? y))
                   (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
                   #f)))))))


