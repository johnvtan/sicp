#!/usr/bin/racket
#lang sicp

(define (cont-frac n d k)
  (if (= 0 k)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i k)
      acc
      (iter (+ i 1) (/ (n i) (+ acc (d i))))))
  (iter 0 0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

