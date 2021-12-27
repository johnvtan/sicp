#!/usr/bin/racket

#lang sicp

(#%require "vectors.rkt")

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))

(add-vect v1 v2)
(sub-vect v2 v1)
(scale-vect 2 v1)