#lang scheme
;;Expected result True PASSES
(define (map f l)
  (if (null? l)
      l
      (if (pair? l)
          (cons (f (car l)) (map f (cdr l)))
          (error "Cannot map over a non-list"))))

(define (deriv a)
  (if (not (pair? a))
      (if (eq? a 'x) 1 0)
      (if (eq? (car a) '+)
          (cons '+
                (map deriv (cdr a)))
          (if (eq? (car a) '-)
              (cons '-
                    (map deriv (cdr a)))
              (if (eq? (car a) '*)
               (list '*
                     a
                     (cons '+
                           (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))
               (if (eq? (car a) '/)
                   (list '-
                         (list '/
                               (deriv (cadr a))
                               (caddr a))
                         (list '/
                               (cadr a)
                               (list '*
                                     (caddr a)
                                     (caddr a)
                                     (deriv (caddr a)))))
                   (error "No derivation method available")))))))

(define result (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
(equal? result '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                  (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                  (* (* b x) (+ (/ 0 b) (/ 1 x)))
                  0))
