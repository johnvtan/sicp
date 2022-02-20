#!/usr/bin/racket

#lang sicp 

(#%require "syntax.rkt")
(#%require "environment.rkt")

; Evaluates each element in exps
; This is how applicative order is implemented
; Before each call to myapply, list-of-values is called
; which myevaluates each of the operands in the arglist
; before the procedure is applied.
; (define (list-of-values exps env)
;   (if (no-operands? exps)
;     '()
;     (cons (myeval (first-operand exps) env)
;           (list-of-values (rest-operands exps) env))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let [(qval (text-of-quotation exp))]
    (lambda (env) qval)))

; has to be done at execution time
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let [(var (assignment-variable exp))
        ; value is constructed (maybe?) at analysis time
        ; unless this is another variable to be looked up at exec time
        (vproc (analyze (assignment-value exp)))]
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let [(var (definition-variable exp))
        (vproc (analyze (definition-value exp)))]
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let [(pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp)))]
    (lambda (env) (if (pproc env) (cproc env) (aproc env)))))

(define (analyze-lambda exp)
  (let [(vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp)))]
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let [(fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp)))]
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond
    [(primitive-procedure? proc)
      (myapply-primitive-procedure proc args)]
    [(compound-procedure? proc)
      ((procedure-body proc)
        (extend-environment (procedure-parameters proc)
          args
          (procedure-environment proc)))]
    [else
      (error "Unknown procedure type: EXECUTE-APPLICATION" proc)]))

(define (analyze exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(quoted? exp) (analyze-quoted exp)]
    [(variable? exp) (analyze-variable exp)]
    [(assignment? exp) (analyze-assignment exp)]
    [(definition? exp) (analyze-definition exp)]
    [(if? exp) (analyze-if exp)]
    [(lambda? exp) (analyze-lambda exp)]
    [(begin? exp) (analyze-sequence (begin-actions exp))]
    [(cond? exp) (analyze (cond->if exp))]
    [(let? exp) (analyze (let->combination exp))]
    [(application? exp) (analyze-application exp)]
    [else (error "Unknown expression type: ANALYZE" exp)]))

(define (myeval exp env)
  ((analyze exp) env))

(define (myapply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(#%provide myeval)
