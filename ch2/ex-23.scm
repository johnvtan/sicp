#!/usr/bin/racket
#lang sicp

(define (my-for-each proc lst)
  (let ([not-used (map proc lst)])
    #t))

(my-for-each (lambda (x) (newline) (display x))
             (list 1 2 3 4))
