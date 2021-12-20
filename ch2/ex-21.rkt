#!/usr/bin/racket
#lang sicp

(define (square-list1 items)
  (if (null? items)
    '()
    (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list1 '(1 2 3 4))
(square-list2 '(1 2 3 4))


