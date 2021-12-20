#!/usr/bin/racket
#lang sicp

; A function f is defined by the rule that f(n) = n if n < 3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
; Write a procedure that computes f by means of a recursive process
; Write a procedure that computes f by means of an interative process

(define (f-rec n)
 (cond
   [(< n 3) n]
   [else (+ (f-rec (- n 1))
            (* 2 (f-rec (- n 2)))
            (* 3 (f-rec (- n 3))))]))

(define (f-iter n)
  (cond 
    [(< n 3) n]
    [else (f-iter-inner 2 n 2 1 0)]))

(define (f-iter-inner curr_n max_n fn fn-1 fn-2)
  (cond
    [(= curr_n max_n) fn]
    [else
      (f-iter-inner (+ curr_n 1)
                    max_n
                    (+ fn (* 2 fn-1) (* 3 fn-2)) ; new fn becomes the definition of the function
                    fn       ; old fn becomes fn-1, old fn-1 becomes fn-2
                    fn-1)]))

(f-rec 3)
(f-rec 4)
(f-rec 5)
(f-rec 6)
(f-rec 7)

(f-iter 3)
(f-iter 4)
(f-iter 5)
(f-iter 6)
(f-iter 7)
