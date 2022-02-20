#!/usr/bin/racket

#lang sicp 

(#%require "time.rkt")
;(#%require "../interpreter/eval.rkt")
;(#%require "../interpreter/environment.rkt")
(#%require "../optimized-interpreter/eval.rkt")
(#%require "../optimized-interpreter/environment.rkt")

(define (repeat f n)
  (if (= n 0)
    '()
    (begin
      (f)
      (repeat f (- n 1)))))

(define fib 
  '(define (fib n)
    (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(define e1 (setup-environment))
(myeval fib e1)


(time
  (repeat (lambda () (myeval '(fib 10) e1)) 1000))

; old: cpu time = 98 ms
; optimized: cpu time = 71 ms