#!/usr/bin/racket

#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (elem acc) (cons (p elem) acc)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (elem acc) (+ acc 1)) 0 sequence))


(define (square x) (* x x))
(map square '(1 2 3 4))
(append '(1 2 3 4) '(5 6 7 8))
(length '(1 2 3 4 5))