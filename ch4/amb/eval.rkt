#!/usr/bin/racket

#lang sicp 

(#%require "syntax.rkt")
(#%require "environment.rkt")

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) 
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let [(qval (text-of-quotation exp))]
    (lambda (env succeed fail) 
      (succeed qval fail))))

; has to be done at execution time
(define (analyze-variable exp)
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let [(var (assignment-variable exp))
        ; value is constructed (maybe?) at analysis time
        ; unless this is another variable to be looked up at exec time
        (vproc (analyze (assignment-value exp)))]
    (lambda (env succeed fail)
      (vproc env
        (lambda (val fail2)
          (let [(old-value (lookup-variable-value var env))]
            (set-variable-value! var val env)
            (succeed 'ok

              ; in event of future failure, we have to undo the assignment
              ; when backtracking
              (lambda ()
                (set-variable-value! var old-value env)
                (fail2)))))
        fail))))

(define (analyze-definition exp)
  (let [(var (definition-variable exp))
        (vproc (analyze (definition-value exp)))]
    (lambda (env succeed fail)
      ; vproc is definition value expr
      ; so call it, and if it succeeds then you actually
      ; define the variable
      (vproc env
        (lambda (val fail2)
          (define-variable! var val env)
          (succeed 'ok fail2))
        fail))))

(define (analyze-if exp)
  (let [(pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp)))]

    (lambda (env succeed fail) 
      (pproc env
        (lambda (pred-value fail2)
          (if pred-value
            (cproc env succeed fail2)
            (aproc env succeed fail2)))
        fail))))

(define (analyze-lambda exp)
  (let [(vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp)))]
    (lambda (env succeed fail) 
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail) 
      (a env
        (lambda (a-value fail2)
          (b env succeed fail2))
        fail)))
  
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))

  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    
    ; Call first element in aprocs
    ((car aprocs) env
      ; if it succeeds, recursively call get-args
      (lambda (arg fail2)
        (get-args (cdr aprocs) env
          ; On success to recursive get-args, add curr arg
          ; to head of list
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
      fail)))

(define (analyze-application exp)
  (let [(fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp)))]
    (lambda (env succeed fail)
      ; fproc is a function that should pass the actual operator
      ; to its succeed function
      (fproc env

        ; So proc contains the actual procedure we want to execute
        (lambda (proc fail2)
          
          ; Now that we have the operator, we want to analyze all the args
          ; which we do with get args
          ; And on success get-args calls succeed with all its argument procs
          ; at which point we can finally call execute-application with the
          ; original succeed func
          (get-args aprocs env
            (lambda (args fail3)
              (execute-application proc args succeed fail3))
            fail2))
        fail))))

(define (analyze-amb exp)
  (let [(cprocs (map analyze (amb-choices exp)))]
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices)
            env
            succeed
            (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (execute-application proc args succeed fail)
  (cond
    [(primitive-procedure? proc)
      (succeed (myapply-primitive-procedure proc args) fail)]
    [(compound-procedure? proc)
      ((procedure-body proc)
        (extend-environment (procedure-parameters proc)
          args
          (procedure-environment proc))
          succeed
          fail)]
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
    [(amb? exp) (analyze-amb exp)]
    [(application? exp) (analyze-application exp)]
    [else (error "Unknown expression type: ANALYZE" exp)]))

(define (myeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (myapply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(#%provide myeval)
