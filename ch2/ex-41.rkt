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

(define (triples-that-sum n s)
  (define (target-sum? triple)
    (= (+ (car triple) (cadr triple) (caddr triple)) s))
  
  (define (all-triples-to-target)
    (flatmap (lambda (k)
                (map (lambda (unique-pair) (cons k unique-pair)) (unique-pairs (- k 1))))
      (enumerate-interval 1 n)))
    
  (filter target-sum? (all-triples-to-target)))

(triples-that-sum 10 20)