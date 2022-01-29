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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-series s1 s2)
  ; First element is the two elements multiplied together
  (cons-stream (* (stream-car s1) (stream-car s2))
  
  ; The rest of the stream is the rest of s2 multiplied by the first thing in s1
  ; I.e. distribute (first s1) over (rest s2)
    (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 

    ; Added to s2 multiplied by the rest of s1
      (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

(define x (mul-series integers (invert-unit-series integers)))
(stream-ref x 0)
(stream-ref x 1)
(stream-ref x 2)
