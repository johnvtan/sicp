#!/usr/bin/racket
#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; note 0 <= pct <= 1
(define (make-center-percent center pct)
  (let [(tolerance (* center pct))]
    (make-center-width center tolerance)))

(define (percent i)
  (/ (width i) (center i)))

(define t1 (make-center-percent 10 0.05))
(percent t1)
(width t1)
(center t1)
  