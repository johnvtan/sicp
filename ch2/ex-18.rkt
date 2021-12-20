#!/usr/bin/racket
#lang sicp

(define (reverse lst)
  (define (inner forward backward)
    (if (null? forward)
      backward
      (inner (cdr forward) (cons (car forward) backward))))
  (inner lst '()))
  
(reverse `(1 2 3 4))
(reverse `(1))
