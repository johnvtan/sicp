#!/usr/bin/racket
#lang sicp

(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

; returns a function that applies f n times to its input 
(define (repeated f n)
  (define (iter acc i)
    (if (= i n)
      acc
      (iter (compose f acc) (+ i 1))))
  (iter (lambda (x) x) 0))

((repeated square 2) 5)
