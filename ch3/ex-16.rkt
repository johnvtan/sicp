#!/usr/bin/racket

#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))


(define x '(a b c))
(count-pairs x) ; -> 3

(define y '(a))
(define lst (cons y y))
(count-pairs (list lst))

(define lst2 (cons lst lst))
(count-pairs lst2)

(define z '(a b))
(set-cdr! (cdr z) z)
(count-pairs z) ; never returns