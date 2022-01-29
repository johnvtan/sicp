#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define s (pairs integers integers))
(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)
(stream-ref s 5)
(stream-ref s 6)