#!/usr/bin/racket
#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define t1 (make-interval -1 4))
(define t2 (make-interval 23 -10))
(lower-bound t1)
(upper-bound t1)

(lower-bound t2)
(upper-bound t2)