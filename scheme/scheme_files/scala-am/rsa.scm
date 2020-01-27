#lang scheme
;; Expected result #t, passes
(define (extended-gcd a b)
  (if (= (modulo a b) 0)
      (cons 0 1)
      (let* ((x:y (extended-gcd b (modulo a b)))
             (x (car x:y))
             (y (cdr x:y)))
        (cons y (- x (* y (quotient a b)))))))
(define (modulo-inverse a n)
  (modulo (car (extended-gcd a n)) n))
(define (totient p q) (* (- p 1) (- q 1)))
(define (square x) (* x x))
(define (modulo-power base exp n)
  (if (= exp 0)
      1
      (if (odd? exp)
          (modulo (* base (modulo-power base (- exp 1) n)) n)
          (modulo (square (modulo-power base (/ exp 2) n)) n))))
(define (is-legal-public-exponent? e p q)
  (and (< 1 e)
       (< e (totient p q))
       (= 1 (gcd e (totient p q)))))
(define (private-exponent e p q)
  (if (is-legal-public-exponent? e p q)
      (modulo-inverse e (totient p q))
      (error "Not a legal public exponent for that modulus.")))
(define (encrypt m e n)
  (if (> m n)
      (error "The modulus is too small to encrypt the message.")
      (modulo-power m e n)))
(define (decrypt c d n)
  (modulo-power c d n))
(define p 41)
(define q 47)
(define n (* p q))
(define e 7)
(define d (private-exponent e p q))
(define plaintext  42)
(define ciphertext (encrypt plaintext e n))
(define decrypted-ciphertext (decrypt ciphertext d n))
(= plaintext decrypted-ciphertext)
