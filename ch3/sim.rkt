#!/usr/bin/racket

#lang sicp

(#%require "circuits/wire.rkt")
(#%require "circuits/gates.rkt")
(#%require "circuits/agenda.rkt")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

(propagate)