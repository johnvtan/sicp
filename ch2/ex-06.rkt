#!/usr/bin/racket
#lang sicp

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define four
  (lambda (f)
    (lambda (x)
      (f (f (f (f x)))))))

; Summary from https://www.cs.rice.edu/~javaplt/311/Readings/supplemental.pdf
; With church numerals, f is the successor function (ie, +1) and x is the zero value
; So to add two church numerals directly, we want to call f a times on the result of calling f b
; times starting from the zero value (which is x).
; The result of calling f b times on x is ((b f) x), which we then use as the zero (or starting)
; value for addition, and so we want to call successor a more times on the result of this to get the
; result of a + b.
(define (internal-add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; To multiply, we want to change the successor function's step size. For integers, the step size is
; 1, but if we use (b f) as our successor function, we can jump in steps of b instead of 1, since (b
; f) returns a function that applies f to x b times.
(define (internal-mul a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

(define (succ x) (+ 1 x))

(define (add a b)
  (((internal-add a b) succ) 0))

(define (mul a b)
  (((internal-mul a b) succ) 0))

(add one two)
(mul two four)

; that's pretty cool
(((internal-mul (internal-add one two) (internal-add one two)) succ) 0)
