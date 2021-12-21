#!/usr/bin/racket
#lang sicp

(define (sqrt x)
  (sqrt-iter 1.0 x 1.0))

(define (sqrt-iter guess x diff)
  (if (good-enough? guess diff)
    guess
    ; wish I remembered how to use let bindings
    (sqrt-iter (improve guess x) x (abs (- (improve guess x) guess)))))
  
(define (good-enough? guess diff)
  (< (/ diff guess) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(* (sqrt 0.0000001) (sqrt 0.0000001))
(* (sqrt 1234567890) (sqrt 1234567890))
