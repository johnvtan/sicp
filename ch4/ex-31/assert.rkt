#!/usr/bin/racket

; Idk how to define macros with #lang sicp
; and I'm using #lang sicp because it has set-car! and set-cdr! defined
; and I can't make test.rkt #lang scheme because there's a difference between
; how #lang sicp and #lang scheme represent quoted expressions I think
#lang scheme

(define-syntax-rule (assert-true expr)
  (when (not expr)
    (error 'assert "assert-true failed -- expr: ~s" (quote expr))))


(define-syntax-rule (assert-eq expr expected)
  (when (not (equal? expr expected))
    (error 'assert "assert-eq failed -- expr: ~s, actual: ~s expected: ~s" (quote expr) expr expected)))

(#%provide assert-eq assert-true)
