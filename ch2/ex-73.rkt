#!/usr/bin/racket

#lang scheme 

; Global op-table + put/get
(define op-table (make-hash))

(define (put op type item)
  (hash-set! op-table (cons op type) item))

(define (get op type)
  (hash-ref op-table (cons op type)))

(define (make-sum a1 a2) 
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))

(define (install-sum-deriv)
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-prod-deriv)
  (define (multiplicand exp) (car exp))
  (define (multiplier exp) (cadr exp))
  
  (define (deriv-prod exp var)
    (make-sum
      (make-product (multiplier exp) (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var) (multiplicand exp))))
  (put 'deriv '* deriv-prod)
  'done)

(define (install-exponent-deriv)
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))

  (define (make-exponent base exponent)
    (define (eval-exp acc n)
      (if (= n 0)
        acc
        (eval-exp (* acc base) (- n 1))))
    (cond
      [(=number? exponent 0) 1]
      [(=number? exponent 1) base]
      [(and (number? base) (number? exponent)) (eval-exp 1 exponent)]
      [else (list '** base exponent)]))
  
  (define (deriv-exponent exp var)
    (make-product
      (make-product (exponent exp) (make-exponent (base exp) (make-sum (exponent exp) -1)))
      (deriv (base exp) var)))
  
  (put 'deriv '** deriv-exponent)
  'done)

(install-sum-deriv)
(install-prod-deriv)
(install-exponent-deriv)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define  (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]

    ; if not number or variable, get the proc from the op-table
    ; based on the operator in the expression (e.g. +, *)
    ; we can't do this for number or variable because they don't
    ; have an operator? they're just some symbol
    ; Maybe we could change (operator exp) to return some other
    ; tag if it encounters a number or variable that we could
    ; use to dispatch? That's more compilcated than this though.
    ;[else ((get 'deriv (operator exp)) (operands exp) var)]))
    [else (let [(proc (get 'deriv (operator exp)))]
            (proc (operands exp) var))]))

(deriv '(* x (+ x 2)) 'x)
(deriv '(+ (** x 2) (* x 3)) 'x)