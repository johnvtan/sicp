#!/usr/bin/racket

#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init
            (accumulate
              (lambda (elem acc) (cons (car elem) acc))
              '() seqs))
          (accumulate-n op init
            (accumulate
              (lambda (elem acc) (cons (cdr elem) acc))
              '() seqs)))))
  
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))