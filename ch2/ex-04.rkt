#!/usr/bin/racket

#lang sicp

; Cons returns a procedure that will take a procedure m as input
; and call it with the arguments x and y
(define (cons x y)
  (lambda (m) (m x y)))

; Car calls z (a procedure returned by cons) and passes in a procedure
; that given two arguments p and q, returns the first
(define (car z)
  (z (lambda (p q) p)))

; So cdr should do the same thing as car (i.e., call z with a procedure that
; takes two arguments) but return the second thing instead.
(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))
(cdr (cons 1 2))

(define test (cons (cons (cons 0 1) 2) 3))
(cdr (car (car test)))