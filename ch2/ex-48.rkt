#!/usr/bin/racket

#lang sicp

(#%require "vectors.rkt")

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define orig (make-vect 0 0))
(define end (make-vect 1 1))
(define seg (make-segment orig end))

(start-segment seg)
(end-segment seg)