#!/usr/bin/racket

#lang sicp

(define (same-parity a . others)
  (define (compare-others cmp-fn others acc)
    (cond
      [(null? others) acc]
      [(cmp-fn (car others)) (compare-others cmp-fn (cdr others) (cons (car others) acc))]
      [else (compare-others cmp-fn (cdr others) acc)]))
  
  (if (even? a)
    (cons a (compare-others even? others '()))
    (cons a (compare-others odd? others '()))))
  
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
