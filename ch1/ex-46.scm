#!/usr/bin/racket
#lang sicp

(define (iterative-improve good-enough? improve-guess)
  (define (func guess)
    (if (good-enough? guess)
      guess 
      (func (improve-guess guess))))
  func)

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (mysqrt x)
  (define (improve-guess guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve-guess) 1.0))

(mysqrt 16)
(mysqrt 64)
(mysqrt 100)

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (improve-guess guess)
    (f guess))
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? improve-guess) first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 3.0)
