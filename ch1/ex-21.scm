#!/usr/bin/racket
#lang sicp

(define (smallest-divisor-simple n)
  (define (divides? a b)
    (= (remainder b a ) 0))
  (define (find-divisor n test)
    (cond 
      [(> (* test test) n) n]
      [(divides? test n) test]
      [else (find-divisor n (+ test 1))]))
  (find-divisor n 2))

(smallest-divisor-simple 199)
(smallest-divisor-simple 1999)
(smallest-divisor-simple 19999)
