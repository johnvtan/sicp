#!/usr/bin/racket

#lang sicp

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (log-base base)
  (lambda (x) 
    (/ (log x) (log base))))
  
(define log2 (log-base 2))
(define log3 (log-base 3))

(define (repeated-div num div-by)
  (if (not (= 0 (remainder num div-by)))
    num
    (repeated-div (/ num div-by) div-by)))

; I guess you could also just count the number of times you can divide 2 into
; z for car (and 3 for cdr) but who wants that
(define (car z)
  (log2 (repeated-div z 3)))

(define (cdr z)
  (log3 (repeated-div z 2)))

(define test (cons 4 5))
(car test)
(cdr test)

(define test2 (cons 0 4))
(car test2)
(cdr test2)