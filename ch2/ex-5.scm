#!/usr/bin/racket
#lang sicp

(define (pow base n)
  (define (helper a b n)
    (cond
      [(= n 0) a]
      [(odd? n) (helper (* a b) b (- n 1))]
      [else (helper a (* b b) (/ n 2))]))
  (helper 1 base n))

; want to represent pairs of nonnegative ints using only numbers and arithmetic operators
(define (mycons x y)
  (* (pow 2 x) (pow 3 y)))

; assumes that if n > div, then n is evenly divisible by div 
(define (count-n-divs n div)
  (define (iter num result)
    (if (not (= (remainder num div) 0))
      result
      (iter (/ num div) (+ result 1))))
  (iter n 0))

(define (mycar z)
  (count-n-divs z 2))

(define (mycdr z)
  (count-n-divs z 3))

(mycar (mycons 2 3))
(mycar (mycdr (mycons 2 (mycons 3 4)))) ; should be 3
(mycdr (mycdr (mycons 2 (mycons 3 4)))) ; should be 4
