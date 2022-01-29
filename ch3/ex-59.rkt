#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")


; a power series is a stream of integers representing coefficients
; for each power
; e.g. (1 2 3 4 ...) -> x + 2x^2 + 3x^3 + 4x^4 + ... +nx^n
(define (integrate-series s)
  (define (construct-stream s factor)
    (cons-stream (/ (stream-car s) factor)
      (construct-stream (stream-cdr s) (+ factor 1))))
  
  (construct-stream s 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(stream-ref exp-series 3)
(stream-ref cosine-series 3)
(stream-ref sine-series 3)