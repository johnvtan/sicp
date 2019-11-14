#!/usr/bin/racket
#lang sicp

(define (double x)
  (+ x x))

(define (halve x)
  (cond
    [(even? x) (/ x 2)]
    [else #f]))

; computes a * b using only 'double' and 'halve' primitives
(define (fast-mult a b)
  (cond 
    [(= b 0) 0]
    [(even? b) (double (fast-mult a (halve b)))]
    [else (+ a (fast-mult a (- b 1)))]))

; ex 1.18
(define (fast-iter-mult a b)
  (define (helper state a b)
    (cond
      [(= b 0) state]
      [(even? b) (helper state (double a) (halve b))]
      [else (helper (+ a state) a (- b 1))]))
  (helper 0 a b))

(fast-mult 3 2)
(fast-mult 3 3)
(fast-mult 42 2)
(fast-mult 2 42)
(fast-mult 10 10)

(fast-iter-mult 3 2)
(fast-iter-mult 3 3)
(fast-iter-mult 42 2)
(fast-iter-mult 2 42)
(fast-iter-mult 10 10)
