#!/usr/bin/racket
#lang sicp

(define (cube-root x)
  (cube-root-iter 1.0 x 1.0))

(define (cube-root-iter guess x diff)
  (if (good-enough? guess diff)
  guess
  (let ((next-guess (improve guess x)))
    (cube-root-iter next-guess x (abs (- next-guess guess))))))
  
(define (good-enough? guess diff)
  (< (/ diff guess) 0.001))

(define (improve guess x)
  (/ 
    (+ 
      (/ x (* guess guess))
      (* 2 guess))
    3))
  
(cube-root 27)
(cube-root 125)
(cube-root 0.0027)
