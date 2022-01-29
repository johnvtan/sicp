#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))


(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

; Because of memo-proc (which I'm assuming is how racket implements this)
; evaluating seq always returns the same values as the first time
; because memo-proc caches the result of each call. If not for memo-proc,
; then each evaluation of seq would change the values because accum changes
; the value of sum.
(stream->list seq); -> seq = '(1, 1+2, 1+2+3, ... , 1+2+3+...+20)
(stream-ref y 7) ; -> returns 7th even number in seq which is 136
(stream->list z)
sum
