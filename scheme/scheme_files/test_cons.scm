#lang scheme 

(equal? (cons '() '(() ())) '(() () ()))