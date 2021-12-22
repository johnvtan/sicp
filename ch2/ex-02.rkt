#!/usr/bin/racket
#lang sicp

; Point helpers
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(") 
  (display (x-point p)) 
  (display ", ") 
  (display (y-point p)) 
  (display ")"))

; Line segment stuff
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg)) (x-point (end-segment seg)))
              (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

(define orig (make-point 0 0))
(define endpoint (make-point 12 14))

(print-point (midpoint-segment (make-segment orig endpoint))) (newline)

