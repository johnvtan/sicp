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

(define (myeval-definition exp env)
  ;(display 'myeval-definition) (newline)
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
  ; (display (list 'apply procedure arguments)) (newline)
  (cond
    [(primitive-procedure? procedure)
      (myapply-primitive-procedure procedure
        (list-of-arg-values arguments env))]
    [(compound-procedure? procedure)
      (let [(processed-params (process-parameters (procedure-parameters procedure) arguments env))]
        (let [(param-names (car processed-params))
              (param-values (cadr processed-params))]
          ; (display (list 'processed-params param-names param-values)) (newline)
          (myeval-sequence
            (procedure-body procedure)
            (extend-environment
              param-names param-values
              (procedure-environment procedure)))))]
    [else
      (error "Unknown procedure type: APPLY" procedure)]))

(define (process-parameters params arguments env)

  (define (param-eager? p) (not (pair? p)))
  (define (param-lazy? p) (and (pair? p) (eq? (cadr p) 'lazy)))
  (define (param-lazy-memo? p) (and (pair? p) (eq? (cadr p) 'lazy-memo)))
  (define (param-name p)
    (cond
      [(param-eager? p) p]
      [(or (param-lazy? p) (param-lazy-memo? p)) (car p)]
      [else (error "Malformed parameter" p)]))

  (define (process-pair param value)
    (cond
      ; eager -- do nothing
      [(param-eager? param) (list param (myeval value env))]
      [(param-lazy? param)
        (list (param-name param) (delay-it value env))]
      [(param-lazy-memo? param)
        (list (param-name param) (delay-it-memo value env))]
      [else (error "Bad parameter" param value)]))

  (if (= (length params) (length arguments))
    (let [(pairs (map process-pair params arguments))]
      (list (map car pairs) (map cadr pairs)))
    (error "Params and arguments not of same length: PROCESS-PARAMETERS" params arguments)))

(define (list-of-arg-values exps env)
  ;(display (list 'list-of-arg-values exps)) (newline)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
      (list-of-arg-values (rest-operands exps) env))))

(define (force-it obj)
  ;(display (list 'force-it obj)) (newline)
  (cond
    [(thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj))]
    [(memo-thunk? obj)
      ; memoization
      (let [(result (actual-value (thunk-exp obj) (thunk-env obj)))]
        (set-car! obj 'evaluated-thunk)
        (set-car! (cdr obj) result)
        (set-cdr! (cdr obj) '())
        result)]
    [(evaluated-thunk? obj) (thunk-value obj)]
    [else obj]))

(#%provide myeval myapply actual-value)