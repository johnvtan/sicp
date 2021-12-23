#!/usr/bin/racket

#lang sicp

(define (square-list items)
  (if (null? items)
    '()
    (cons (* (car items) (car items)) (square-list (cdr items)))))
  
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list '(1 2 3 4 5))
(square-list2 '(1 2 3 4 5))