#!/usr/bin/racket

#lang scheme

(#%require "numbers.rkt")

(=zero? 0)
(=zero? 4)

(=zero? (make-rational 0 5))
(=zero? (make-rational 1 6))

(=zero? (make-complex-from-mag-ang 0 23))
(=zero? (make-complex-from-mag-ang 1 23))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-real-imag 0 1))