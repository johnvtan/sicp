#!/usr/bin/racket

#lang sicp

(define (reverse lst)
  (define (iter acc lst)
    (if (null? lst)
    acc
    (iter (cons (car lst) acc) (cdr lst))))
  (iter '() lst))

(reverse '(1 2 3 4))
(reverse '(1))
(reverse '())