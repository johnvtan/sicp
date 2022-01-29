#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; returns (3, 7, 5, 0, 0, ...)
; this does long division and returns the decimal places to the right
; in the given radix
(stream-ref (expand 3 8 10) 2)
