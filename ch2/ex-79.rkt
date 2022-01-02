#!/usr/bin/racket

#lang scheme

(#%require "numbers.rkt")

(equ? 3 4)
(equ? 3 3)

(define r1 (make-rational 4 5))
(define r2 (make-rational 4 6))
(define also-r2 (make-rational 2 3))

(equ? r1 r2)
(equ? r2 also-r2)

(define z1 (make-complex-from-real-imag 4 5))
(define z2 (make-complex-from-real-imag 3 1))
(define also-z1 (add z2 (make-complex-from-real-imag 1 4)))
(equ? z1 z2)
(equ? z1 also-z1)

(define z3 (make-complex-from-real-imag 4 0))
(equ? 4 z3)
(equ? z3 4)
(equ? z1 10)

(define r3 (make-rational 40 10))
(equ? 4 r3)
(equ? r3 4)
(equ? r3 5)

; this doesn't work because of precision issues during conversion?
;(define also-z1 (make-complex-from-mag-ang (magnitude z1) (angle z1)))
;z1
;(real-part also-z1)
;(imag-part also-z1)
;(equ? z1 also-z1)

