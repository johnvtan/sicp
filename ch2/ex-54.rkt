#!/usr/bin/racket

#lang sicp

(define (my-equal? l1 l2)
  (cond
    [(null? l1) (null? l2)]
    [(eq? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2))]
    [else #f]))

(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list) '(this (is a) list))