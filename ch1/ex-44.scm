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

(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (/ 
      (+ (f x) (f (+ x dx)) (f (- x dx)))
      3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
