#!/usr/bin/racket
#lang sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ([next (f guess)])
      (if (close-enough? next guess)
        next
        (try next))))
  (try first-guess))

(define (test-func x) (/ (log 1000) (log x)))

(display "no damping")
(newline)
(fixed-point test-func 5.0)

(define (average x y) (/ (+ x y) 2))
(newline)
(display "with damping")
(newline)
; less steps - damping makes it faster
(fixed-point (lambda (x) (average x (test-func x))) 5.0)
