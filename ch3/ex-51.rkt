#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define (show x)
  (display x) (newline)
  'bleh)

(define x (stream-map show (stream-enumerate-interval 0 10)))

; prints 0->5 since stream is evaluated until 5th element
(stream-ref x 5)

; then prints 6, 7 because stream continues from last eval
(stream-ref x 7)

