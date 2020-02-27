#lang scheme

(require "imports.scm")

(define x "str")
(set! x #f)
(define y #t)

(or x y y)