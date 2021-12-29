#!/usr/bin/racket

#lang sicp

; O(n), n is likely to be larger than previous representation
; bc of duplicates
(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

; 0(1) operation now
(define (adjoin-set x set)
  (cons x set))

; O(m) where m is len(s2)
(define (union-set s1 s2)
  (append s1 s2))

(define (intersection-set s1 s2)
  (cond
    [(or (null? s1) (null? s2)) '()]
    [(element-of-set? (car s1) s2)
      (cons (car s1) (intersection-set (cdr s1) s2))]
    [else (intersection-set (cdr s1) s2)]))

; I guess I'd use this if I'm adding to the set a lot, but searching very infrequently?
