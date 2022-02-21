#!/usr/bin/racket

#lang sicp 

(#%require "eval.rkt")
(#%require "environment.rkt")
(#%require "syntax.rkt")

(define input-prompt ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define (driver-loop)
  (display input-prompt)
  (let [(input (read))]
    (display (list input (list? input))) (newline)
    (let [(output (actual-value input the-global-environment))]
      (display output-prompt)
      (display output) (newline) (newline)))
  (driver-loop))

(driver-loop)
