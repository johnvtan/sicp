#!/usr/bin/racket

#lang sicp 

; Representing expressions
(define (self-evaluating? exp)
  (cond
    [(number? exp) #t]
    [(string? exp) #t]
    [else #f]))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; Tagged lists are lists where the first element is the given tag
; So the syntax of special forms like set!/define/quote are defined
; largely by this structure.
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    ; For variable defines like (define x 1)
    (cadr exp)

    ; If we define a function the syntax is
    ; (define (func-name arglist...))
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)

    ; cdadr exp -> arglist
    ; cddr exp -> body
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
  
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]))
  
(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let [(first (car clauses))
          (rest (cdr clauses))]
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clauses isn't last: COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (and? exp) (tagged-list? exp 'and))

; and is derived as nested if expressoins like
; (if (and1)
;   (if (and2)
;     ...
;      (if (andn) true false))
;    false)
;   false)
(define (and->if exp)
  (define (expand exprs)
    (if (null? exprs)
      'true
      (make-if (car exprs)
               (expand (cdr exprs))
               'false)))
  (expand (cdr exp)))

; or is derived as nested if expressions like
; (if (or1) true
;   (if (or2) true false)
;  false)
(define (or? exp) (tagged-list? exp 'or))
(define (or->if exp)
  (define (expand exprs)
    (if (null? exprs)
      'false
      (make-if (car exprs)
                'true
                (expand (cdr exprs)))))
  (expand (cdr exp)))

(#%provide self-evaluating? variable?
  quoted? text-of-quotation
  if?
  first-operand rest-operands no-operands?
  lambda? make-procedure
  if-predicate if-consequent if-alternative
  first-exp rest-exps last-exp?
  assignment-variable assignment-value assignment?
  definition-variable definition-value definition?
  lambda? lambda-parameters lambda-body make-lambda
  begin-actions begin? make-begin
  operator operands
  procedure-parameters procedure-body procedure-environment
  cond? cond->if
  application?
  primitive-procedure?
  compound-procedure?
  primitive-implementation
  and? and->if 
  or? or->if)