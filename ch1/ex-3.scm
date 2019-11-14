#!/usr/bin/racket
#lang sicp

(define (sum-squares a b)
  (+ (* a a) (* b b)))

(define (larger-sum-of-squares a b c)
  (cond 
    [(and (<= a b) (<= a c)) (sum-squares b c)]
    [(and (<= b a) (<= b c)) (sum-squares a c)]
    [else (sum-squares a b)]))

(= (larger-sum-of-squares 1 2 3) (sum-squares 2 3))
(= (larger-sum-of-squares 4 4 5) (sum-squares 4 5))
(= (larger-sum-of-squares 4 4 4) (sum-squares 4 4))
