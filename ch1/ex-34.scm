#!/usr/bin/racket
#lang sicp

(define (f g) (g 2))

(define (square x) (* x x))

(f square)

; this will call f with argument 2, which will try to execute the expression (2 2), then result in
; an error because 2 isn't a procedure that takes arguments 
(f f)
