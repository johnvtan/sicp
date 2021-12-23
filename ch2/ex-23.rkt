#!/usr/bin/racket

#lang sicp

(define (for-each do items)
  (cond
    [(null? items) #t]
    [else
      (do (car items))
      (for-each do (cdr items))]))

(for-each (lambda (x) (newline) (display x))
          '(57 321 88))
