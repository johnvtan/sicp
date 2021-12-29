#!/usr/bin/racket

#lang sicp

(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (union-set s1 s2)
  (if (null? s1)
    s2
    (union-set (cdr s1) (adjoin-set (car s1) s2))))

(union-set '(1 2 3 4) '(5 6 7 8))
(union-set '(1 2 3 4) '(3 4 5 6))