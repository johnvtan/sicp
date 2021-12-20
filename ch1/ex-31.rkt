#!/usr/bin/racket
#lang sicp

(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (ident x) x)
(define (inc x) (+ x 1))

(define (factorial-iter n)
  (product-iter ident 1 inc n))

(define (factorial-rec n)
  (product-rec ident 1 inc n))

(factorial-iter 6)
(factorial-rec 6)

(define (approx-pi n)
  (define (numer n)
    (+ 2 (* 2 (ceiling (/ n 2)))))
  (define (denom n)
    (+ 3 (* 2 (floor (/ n 2)))))
  (define (term n) (/ (numer n) (denom n)))
  (* 4.0 (product-iter term 0 inc n)))

(approx-pi 50)
(approx-pi 100)
(approx-pi 1000)
(approx-pi 10000)
