#!/usr/bin/racket

#lang scheme

(define (list-values-lr exps env)
  (if (no-operands exps)
    '()
    (let [(first-exp (eval (first-operand exps) env))]
      (let [(rest-exps (list-values-lr (rest-operands exps) env))]
        (cons first-exp rest-exps)))))
; r->l is the same, but the lets swapped