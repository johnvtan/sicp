#!/usr/bin/racket
#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result  
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (applied-f k)
    (f (+ a (* k h))))
  (define (term k)
    (cond
      [(or (= k 0) (= k n)) (applied-f k)]
      [(odd? k) (* 4 (applied-f k))]
      [else (* 2 (applied-f k))]))
  (define (inc x) (+ x 1))
  (* (/ h 3.0) (sum term 0 inc n)))

;(simpson-integral (lambda (x) (* x x x)) 0 1 100)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

(integral cube 0 4 0.01)
(simpson-integral cube 0 4 100)

(integral cube 0 1 0.01)
