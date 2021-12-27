#!/usr/bin/racket

#lang sicp

(#%require "../common-ops.rkt")

; Generates the sequence of pairs (i, j)
; with 1 < j < i <= n
(define (unique-pairs n)
  (flatmap 
    (lambda (i) 
      (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 8)