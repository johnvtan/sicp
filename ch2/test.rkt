#!/usr/bin/racket
#lang sicp

(#%require "rat.scm")

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (div-rat one-half one-third))

(print-rat (make-rat 4 8))
(print-rat (make-rat -4 -8))
(print-rat (make-rat -4 8))
(print-rat (make-rat 4 -8))
(print-rat (make-rat -1 1))
(print-rat (make-rat 16 -4))
