#!/usr/bin/racket

#lang sicp

; I always get confused with fold-right/left.
; I would think fold-right would fold from left->right, but
; really it's going right->left, since it starts at the end of
; the list and works its way backwards to the front.
(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))
  
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      ; Why would the book switch the order of arguments passed to op?
      ; I don't think this is a good decision -- it causes confusion for no gain.
      ; Is it really clearer that, for operations passed to fold-left, the accumulator
      ; is the left-most parameter? I don't think it makes sense. It would be simpler
      ; if ops were interchangeable between fold-left/right and that the argument order
      ; is consistent.
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (reverse-right seq)
  (fold-right (lambda (elem acc) (append acc (list elem))) nil seq))

(define (reverse-left seq)
  (fold-left (lambda (acc elem) (cons elem acc)) nil seq))

(reverse-right '(1 2 3 4))
(reverse-left '(1 2 3 4))