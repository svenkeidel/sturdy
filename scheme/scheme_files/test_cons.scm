#lang racket 

(equal? (cons '() '(() ())) '(() () ()))