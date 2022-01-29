#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(stream-ref factorials 10)