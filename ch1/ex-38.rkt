#!/usr/bin/racket
#lang sicp

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ acc (d i))))))
  (iter (- k 1) (/ (n k) (d k))))

(define (n i) 1.0)
(define (d i) 
  (cond
    [(= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))]
    [else 1]))

(+ 2 (cont-frac n d 100))
