#!/usr/bin/racket

#lang sicp

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< x (car set)) (cons x set)]
    [(= x (car set)) set]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

(define (union-set s1 s2)
  (cond
    [(null? s1) s2]
    [(null? s2) s1]

    ; If (car s1) < (car s2) -> (s1, union-set(rest of s1, s2))
    [(< (car s1) (car s2)) 
      (cons (car s1) (union-set (cdr s1) s2))]

    [(> (car s1) (car s2)) 
      (cons (car s2) (union-set (cdr s1) (cdr s2)))]

    [(= (car s1) (car s2)) 
      (cons (car s1) (union-set (cdr s1) (cdr s2)))]

    [else (error "UNREACHABLE")]))

(union-set '(1 2 3 4) '(1 2 3 4))
(union-set '(1 2 3 4) '(4 5 6 7))
 
    
    