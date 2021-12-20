#!/usr/bin/racket
#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))
((double inc) 4)

(((double double) inc) 5)
(((double (double double)) inc) 5)
