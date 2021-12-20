#!/usr/bin/racket
#lang sicp

; recursive version
;(define (accumulate combiner null-val term a next b)
;  (if (> a b)
;    null-val
;    (combiner (term a) (accumulate combiner null-val term (next a) next b))))

; iterative version
(define (accumulate combiner null-val term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-val))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)

(define (approx-pi n)
  (define (numer n)
    (+ 2 (* 2 (ceiling (/ n 2)))))
  (define (denom n)
    (+ 3 (* 2 (floor (/ n 2)))))
  (define (term n) (/ (numer n) (denom n)))
  (* 4.0 (product term 0 inc n)))

(approx-pi 1000)
