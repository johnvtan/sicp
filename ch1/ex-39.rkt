#!/usr/bin/racket

#lang sicp

(define (cont-frac n d k)
  (if (= 0 k)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ acc (d i))))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-iter
    (lambda (i) (if (= 1 i) x (- (* x x))))
    (lambda (i) (- (* i 2) 1))
    k))

;(tan-cf 0.0 100)
(tan-cf 3.14 100)
(tan-cf (/ 3.14 3) 100)
(tan-cf (/ 3.14 4) 100)