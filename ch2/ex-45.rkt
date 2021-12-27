#!/usr/bin/racket

#lang sicp

(#%require sicp-pict)

(define (split first-op second-op)
  (define (inner painter n)
    (if (= n 0)
      painter
      (let [(smaller (inner painter (- n 1)))]
        (first-op painter (second-op smaller smaller)))))
  inner)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (up-split einstein 4))
(paint (right-split einstein 4))