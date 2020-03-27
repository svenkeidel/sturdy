(let ((f (lambda (x) (x)))
      (g (lambda () 1))
      (h (lambda () 2.3)))
    (+ (f g) (f h)))