#!/usr/bin/racket

#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate
    (lambda (elem acc)
      (if (pair? elem)
        (+ acc (count-leaves elem))
        (+ acc 1)))
    0
    t))

(count-leaves '(1 2 3 4))
(count-leaves '(1 (2 (3 (4 5)))))