#!/usr/bin/racket
#lang sicp

(define (p) (p))

(define (test x y)
    (if (= x 0)
         0
         y))

(test 0 (p))

; in applicative order, because the interpreter tries to evaluate the args to test first, and in this case p is
; recursively defined, the interpreter will loop infinitely as it continually looks up the definition of p only to have
; to look up the definition of p over and over again

; in a normal order evaluation interpreter, the args are only evaluated when they are needed. because the test function
; does not use its second argument, (p) is never evaluated and so the expr will return 0
