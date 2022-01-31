#!/usr/bin/racket

#lang sicp 

(#%require "syntax.rkt")
(#%require "environment.rkt")
(#%require "hash.rkt") ; hmmmmmmm

(define eval-table (make-hash))
(define (put type proc)
  (hash-set! eval-table type proc))
(define (get type)
  (hash-ref eval-table type #f))

; Evaluates each element in exps
; This is how applicative order is implemented
; Before each call to myapply, list-of-values is called
; which myevaluates each of the operands in the arglist
; before the procedure is applied.
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (myeval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

; Notive how this only myevaluates either the consequent or the alternative
; unlike procedure application.
; Note also that this assumes each if expr has both a consequent and alternative
; unlike some previous uses which only had an alternative.
(define (myeval-if exp env)
  (if (myeval (if-predicate exp) env)
    (myeval (if-consequent exp) env)
    (myeval (if-alternative exp) env)))

(define (myeval-sequence exps env)
  (cond
    [(last-exp? exps) (myeval (first-exp exps) env)]
    [else
      (myeval (first-exp exps) env)
      (myeval-sequence (rest-exps exps) env)]))

(define (myeval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (myeval (assignment-value exp) env)
                       env)
  'ok)

(define (myeval-definition exp env)
  ;(display 'myeval-definition) (newline)
  (define-variable! (definition-variable exp)
                    (myeval (definition-value exp) env)
                    env))

(define (myeval-quote exp env)
  (text-of-quotation exp))

(define (myeval-lambda exp env)
  (make-procedure (lambda-parameters exp) (lambda-body exp) env))

(define (myeval-begin exp env)
  (myeval-sequence (begin-actions exp) env))

(define (myeval-cond exp env)
  (myeval (cond->if exp) env))

(put 'quote myeval-quote)
(put 'set! myeval-assignment)
(put 'define myeval-definition)
(put 'if myeval-if)
(put 'lambda myeval-lambda)
(put 'begin myeval-begin)
(put 'cond myeval-cond)

(define (myeval exp env)
  ;(display (list 'Evaluating exp (symbol? exp))) (newline)
  (cond

    ; Primitives?
    [(self-evaluating? exp) exp]

    ; Lookup the variable value in the environment
    ; Are primitive operators (+, -, etc) variables?
    [(variable? exp) (lookup-variable-value exp env)]

    ; DATA DIRECTED!!
    [(get (car exp)) ((get (car exp)) exp env)]

    [(application? exp)
      (myapply (myeval (operator exp) env)
             (list-of-values (operands exp) env))]
    [else (error "Unknown expression type EVAL" exp)]))

(define (myapply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (myapply procedure arguments)
  (cond
    [(primitive-procedure? procedure)
      (myapply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
      (myeval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)))]
    [else
      (error "Unknown procedure type: APPLY" procedure)]))

(#%provide myeval myapply)
