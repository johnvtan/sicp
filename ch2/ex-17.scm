#!/usr/bin/racket
#lang sicp

(define (last-pair lst)
  (if (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))))

(last-pair `(1 2 3 4))
(last-pair `(1))
