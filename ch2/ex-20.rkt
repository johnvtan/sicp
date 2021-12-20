#!/usr/bin/racket
#lang sicp

(define (same-parity x . y)
  (define (get-parity parity? lst result)
    (if (null? lst)
      result
      (let ([first (car lst)])
        (get-parity parity? (cdr lst)
                    (if (parity? first)
                      (cons first result)
                      result)))))
  (get-parity (if (even? x) even? odd?) y '()))

(same-parity 1 1 2 3 4)
(same-parity 2 12 32 42 3 4)
    
