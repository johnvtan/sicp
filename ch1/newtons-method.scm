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

(define (square x) (* x x))

(define (new-sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(new-sqrt 9.0)
