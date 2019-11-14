#!/usr/bin/racket
#lang sicp

(define (iter-log-exp base n)
  (define (helper a b n)
    (cond
      [(= n 0) a]
      [(odd? n) (helper (* a b) b (- n 1))]
      [else (helper a (* b b) (/ n 2))]))
  (helper 1 base n))

(iter-log-exp 3 2)
(iter-log-exp 3 3)
(iter-log-exp 2 6)
(iter-log-exp 4 4)
