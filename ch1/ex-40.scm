#!/usr/bin/racket
#lang sicp

(define dx 0.00001)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? next guess)
        next
        (try next))))
  (try first-guess))

; returns a function f(x) = [g(x+dx) - g(x)]/dx
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; f(x) = x - [g(x)/g'(x)]
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (cubic a b c) 
  (lambda (x) 
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 1 2 3) 1.0)
