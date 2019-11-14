#!/usr/bin/racket
#lang sicp

(define (euclid-gcd a b)
  (if (= b 0)
    a
    (euclid-gcd b (remainder a b))))

(euclid-gcd 206 48)
(euclid-gcd 100 22)
(euclid-gcd 25 100)
(euclid-gcd 28 16)
