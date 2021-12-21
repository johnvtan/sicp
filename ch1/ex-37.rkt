#!/usr/bin/racket
#lang sicp

(define (cont-frac n d k)
  (if (= 0 k)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))


; This was the old version from over a year ago,
; but I think it's wrong. It seems like it's going the opposite direction
; and like inverts the computation? Not sure how to describe it, but it's
; important that we go from k->0, not 0->k, so that at the end the numerator
; at the very top is the value of (n 1).
; I guess it didn't matter for this exercise because neither n or d changed
; with i.
;(define (cont-frac-iter n d k)
;  (define (iter i acc)
;    (if (= i k)
;      acc
;      (iter (+ i 1) (/ (n i) (+ acc (d i))))))
;  (iter 0 0))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ acc (d i))))))
  (iter k 0))


(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

