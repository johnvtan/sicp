#!/usr/bin/racket

#lang sicp 

(define (has-cycle? x)
  (define (iter p1 p2)
    (cond
      [(eq? p1 p2) #t]
      [(or (null? p1) (null? p2)) #f]
      [(or (null? (cdr p2)) (null? (cdr (cdr p2)))) #f]
      [else (iter (cdr p1) (cdr (cdr p2)))]))
  (if (null? x)
    #f
    (iter x (cdr x))))

(has-cycle? '(a b c d))

(define z '(a b c))
(set-cdr! (cdr z) z)
(has-cycle? z)

(has-cycle? '(a))
(has-cycle? '())
(define y '(a))
(set-cdr! y y)
y
(has-cycle? y)