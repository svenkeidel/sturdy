(define (lookup key table)
  (begin
    (define (loop x)
      (if (null? x) #f
          (let ((pair (car x)))
            (if (eq? (car pair) key)
                pair
                (loop (cdr x))))))
    (loop table)))

(define properties '())

(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y (cdr y) #f))
        #f)))

(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
        (let ((y (lookup key2 (cdr x))))
          (if y (set-cdr! y val)
              (set-cdr! x (cons (cons key2 val) (cdr x)))))
        (set! properties (cons (list key1 (cons key2 val)) properties)))))

(define *current-gensym* 0)

(define (generate-symbol)
  (set! *current-gensym* (+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))

(define (append-to-tail! x y)
  (if (null? x)
      y
      (begin
        (define ($do-loop0 a b)
          (if (null? b)
              (begin
                (set-cdr! a y)
                x)
              (begin ($do-loop0 b (cdr b)))))
        ($do-loop0 x (cdr x)))))

(define (tree-copy x)
  (if (not (pair? x))
      x
      (cons (tree-copy (car x))
            (tree-copy (cdr x)))))

(define *rand* 21)

(define (init n m npats ipats)
  (let ((ipats (tree-copy ipats)))
    (begin
      (define ($do-loop1 p)
        (if (null? (cdr p))
            (set-cdr! p ipats)
            (begin
              ($do-loop1 (cdr p)))))
      ($do-loop1 ipats))
    (begin
      (define ($do-loop2 n i name a)
        (if (= n 0)
            a
            (begin
              (set! a (cons name a))
              (begin
                (define ($do-loop3 i)
                  (if (zero? i)
                      (begin)
                      (begin (put name (generate-symbol) #f)
                             ($do-loop3 (- i 1)))))
                ($do-loop3 i))
              (put name 'pattern
                   (begin
                     (define ($do-loop4 i ipats a)
                       (if (zero? i)
                           a
                           (begin
                             (set! a (cons (car ipats) a))
                             ($do-loop4 (- i 1) (cdr ipats) a))))
                     ($do-loop4 npats ipats '())))
              (begin
                (define ($do-loop5 j)
                  (if (zero? j)
                      (begin)
                      (begin
                        (put name (generate-symbol) #f)
                        ($do-loop5 (- j 1)))))
                ($do-loop5 (- m i)))
              ($do-loop2 (- n 1)
                         (cond ((zero? i) m) (else (- i 1)))
                         (generate-symbol)
                         a))))
      ($do-loop2 n m (generate-symbol) '()))))

(define (browse-random)
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)

(define (randomize l)
  (begin
    (define ($do-loop6 a)
      (if (null? l)
          a
          (begin
            (let ((n (remainder (browse-random) (length l))))
              (cond
               ((zero? n) (set! a (cons (car l) a)) (set! l (cdr l)) l)
               (else
                (begin
                  (define ($do-loop7 n x)
                    (if (= n 1)
                        (begin
                          (set! a (cons (cadr x) a))
                          (set-cdr! x (cddr x))
                          x)
                        (begin
                          ($do-loop7 (- n 1) (cdr x)))))
                  ($do-loop7 n l)))))
            ($do-loop6 a))))
    ($do-loop6 '())))

(define (my-match pat dat alist)
  (cond
   ((null? pat) (null? dat))
   ((null? dat) '())
   ((or (eq? (car pat) '?) (eq? (car pat) (car dat))) (my-match (cdr pat) (cdr dat) alist))
   ((eq? (car pat) '*) (or (my-match (cdr pat) dat alist) (my-match (cdr pat) (cdr dat) alist) (my-match pat (cdr dat) alist)))
   (else
    (cond
     ((not (pair? (car pat)))
      (cond
       ((eq? (string-ref (symbol->string (car pat)) 0) #\?)
        (let ((val (assq (car pat) alist)))
          (cond (val (my-match (cons (cdr val) (cdr pat)) dat alist))
                (else (my-match (cdr pat) (cdr dat) (cons (cons (car pat) (car dat)) alist))))))
       ((eq? (string-ref (symbol->string (car pat)) 0) #\*)
        (let ((val (assq (car pat) alist)))
          (cond
           (val (my-match (string-append (cdr val) (cdr pat)) dat alist))
           (else
            (begin
              (define ($do-loop8 l e d)
                (if (or (null? e) (my-match (cdr pat) d (cons (cons (car pat) l) alist)))
                    (if (null? e) #f #t)
                    (begin
                      ($do-loop8 (append-to-tail! l (cons (if (null? d) '() (car d)) '()))
                                 (cdr e)
                                 (if (null? d) '() (cdr d))))))
                 ($do-loop8 '() (cons '() dat) dat))))))
          (else #f)))
     (else (and (pair? (car dat))
                (my-match (car pat) (car dat) alist)
                (my-match (cdr pat) (cdr dat) alist)))))))
(define database
  (randomize
   (init 100 10 4 '((a a a b b b b a a a a a b b a a a) (a a b b b b a a (a a) (b b)) (a a a b (b a) b a b a)))))

(define (browse pats) (investigate database pats))

(define (investigate units pats)
  (begin
    (define ($do-loop9 units)
      (if (null? units)
          (begin)
          (begin
            (begin
              (define ($do-loop10 pats)
                (if (null? pats)
                    (begin)
                    (begin
                      (begin
                        (define ($do-loop11 p)
                          (if (null? p)
                              (begin)
                              (begin (my-match (car pats) (car p) '()) ($do-loop11 (cdr p)))))
                        ($do-loop11 (get (car units) 'pattern)))
                      ($do-loop10 (cdr pats)))))
              ($do-loop10 pats))
            ($do-loop9 (cdr units)))))
    ($do-loop9 units)))

(begin
  (browse '((*a ?b *b ?b a *a a *b *a) (*a *b *b *a (*a) (*b)) (? ? * (b a) * ? ?)))
  *current-gensym*)
