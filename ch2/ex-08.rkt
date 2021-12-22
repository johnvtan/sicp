#!/usr/bin/racket

#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Lowest possible value in subtracted interval is (lower x) - (upper y)
; and highest is (upper x) - (lower y)
(define (sub-interval x y) 
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))) 

(define t1 (make-interval 1 4))
(define t2 (make-interval 10 23))

(sub-interval t1 t2)
(sub-interval t2 t1)
