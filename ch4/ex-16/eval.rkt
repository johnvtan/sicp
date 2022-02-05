#!/usr/bin/racket

#lang sicp 

(#%require "syntax.rkt")
(#%require "environment.rkt")

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
  (display (list 'myeval-sequence exps)) (newline)
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

(define (myeval exp env)
  (display (list 'Evaluating exp)) (newline)
  (cond

    ; Primitives?
    [(self-evaluating? exp) exp]

    ; Lookup the variable value in the environment
    ; Are primitive operators (+, -, etc) variables?
    [(variable? exp) (lookup-variable-value exp env)]

    ; Quotes get the text of the symbol, e.g. 'text -> text
    [(quoted? exp) (text-of-quotation exp)]

    [(assignment? exp) (myeval-assignment exp env)]
    [(definition? exp) (myeval-definition exp env)]
    [(if? exp) (myeval-if exp env)]

    ; Creates a procedure, which can be passed to myapply
    [(lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env)]
    [(begin? exp) (myeval-sequence (begin-actions exp) env)]

    ; Converts cond statements into ifs
    [(cond? exp) (myeval (cond->if exp) env)]

    [(let? exp) 
      (myeval (let->combination exp) env)]

    ; Note how the operator is also myevaluated
    ; This is how ((if x + -) 3 2) would be myevaluated
    [(application? exp)
      (myapply (myeval (operator exp) env)
             (list-of-values (operands exp) env))]
    [else (error "Unknown expression type EVAL" exp)]))

(define (myapply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (myapply procedure arguments)
  (cond
    [(primitive-procedure? procedure)
      (display (list 'apply-primitive arguments procedure)) (newline)
      (myapply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
      (display (list 'apply arguments (procedure-body procedure))) (newline)
      (myeval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)))]
    [else
      (error "Unknown procedure type: APPLY" procedure)]))

(#%provide myeval myapply)
