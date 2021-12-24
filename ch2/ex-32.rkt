#!/usr/bin/racket

#lang sicp

(define (subsets s)
  (if (null? s)
    (list nil)
    (let [(rest (subsets (cdr s)))]
      ; Let c be the first element in s
      ; All the subsets of s are (subsets of s without c) + (subsets of s with c)
      ; 'rest' here are all the subsets of s without c since 'subsets' is called on (cdr s)
      ; all the subsets of s with c can be found by appending c to each subset in 'rest'
      (append rest (map (lambda (elem) (cons (car s) elem)) rest)))))

(subsets '(1))
(subsets '(1 2))
(subsets '(1 2 3))
