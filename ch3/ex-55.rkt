#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define (partial-sums s)
  (cons-stream (stream-car s)
    (stream-map + (stream-cdr s) (partial-sums s))))

(stream-ref (partial-sums integers) 4)