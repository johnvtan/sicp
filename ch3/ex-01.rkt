#!/usr/bin/racket

#lang scheme

(define (make-accumulator acc)
  (lambda (to-add)
    (set! acc (+ acc to-add))
    acc))

(define a1 (make-accumulator 1))
(define a2 (make-accumulator 10))

(a1 10)
(a1 3)

(a2 23)
(a2 4)