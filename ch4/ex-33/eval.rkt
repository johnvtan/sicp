#!/usr/bin/racket

#lang sicp 

(#%require "syntax.rkt")
(#%require "environment.rkt")

(define (make-cons a b) (list 'cons a b))
(define (list->cons lst)
  (if (null? lst)
    '()
    (make-cons (car lst) (list->cons (cdr lst)))))

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
  (if (actual-value (if-predicate exp) env)
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

; TODO redefining primitves doesn't work...
(define (myeval-definition exp env)
  ;(display (list 'myeval-definition (definition-variable exp) (definition-value exp))) (newline)
  (define-variable! (definition-variable exp)
                    (myeval (definition-value exp) env)
                    env))

(define (myeval exp env)
  ;(display (list 'Evaluating exp)) (newline)
  (cond

    ; Primitives?
    [(self-evaluating? exp) exp]

    ; Lookup the variable value in the environment
    ; Are primitive operators (+, -, etc) variables?
    [(variable? exp) (lookup-variable-value exp env)]

    ; Quotes get the text of the symbol, e.g. 'text -> text
    [(quoted? exp) (myeval (list->cons (text-of-quotation exp)) env)]

    [(assignment? exp) (myeval-assignment exp env)]
    [(definition? exp) 
      (myeval-definition exp env)]
      ;(display (list 'definition exp env)) (newline)]
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
    
    [(unless? exp)
      (myeval (unless->if exp) env)]

    ; Note how the operator is also myevaluated
    ; This is how ((if x + -) 3 2) would be myevaluated
    [(application? exp)
      (myapply (actual-value (operator exp) env)
               (operands exp) env)]
    [else (error "Unknown expression type EVAL" exp)]))

(define (actual-value exp env)
  (force-it (myeval exp env)))

(define (myapply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (myapply procedure arguments env)
  (cond
    [(primitive-procedure? procedure)
      ;(display (list 'primitive-procedure procedure arguments)) (newline)
      (myapply-primitive-procedure procedure
        (list-of-arg-values arguments env))]
    [(compound-procedure? procedure)
      (myeval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          (list-of-delayed-args arguments env)
          (procedure-environment procedure)))]
    [else
      (error "Unknown procedure type: APPLY" procedure)]))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
      (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
      (list-of-delayed-args (rest-operands exps) env))))

(define (force-it obj)
  (cond
    [(thunk? obj)
      ; memoization
      (let [(result (actual-value (thunk-exp obj) (thunk-env obj)))]
        (set-car! obj 'evaluated-thunk)
        (set-car! (cdr obj) result)
        (set-cdr! (cdr obj) '())
        result)]
    [(evaluated-thunk? obj) (thunk-value obj)]
    [else obj]))

; (define (force-it obj)
;   (if (thunk? obj)
;     (actual-value (thunk-exp obj) (thunk-env obj))
;     obj))

(#%provide myeval myapply actual-value)
