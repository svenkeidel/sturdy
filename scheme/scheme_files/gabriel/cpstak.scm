;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: 6
(letrec ((tak (lambda (x y z k)
                (if (not (< y x))
                  (k z)
                  (tak (- x 1)
                       y
                       z
                       (lambda (v1)
                         (tak (- y 1)
                              z
                              x
                              (lambda (v2)
                                (tak (- z 1)
                                     x
                                     y
                                     (lambda (v3)
                                       (tak v1 v2 v3 k)))))))))))
  (tak 20 10 5 (lambda (a) a)))
