#!/usr/bin/racket

#lang sicp

(define (cc amount coin-list)
  (cond
    [(= amount 0 ) 1]
    [(or (< amount 0) (null? coin-list)) 0]
    [else (+ (cc amount (cdr coin-list))
             (cc (- amount (car coin-list)) coin-list))]))


(define us-coins '(25 50 10 5 1))
(define uk-coins '(50 100 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)
