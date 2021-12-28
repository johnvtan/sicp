#!/usr/bin/racket

#lang sicp
(#%require "../common-ops.rkt")

; Selectors/data representation for expressions
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define  (=number? exp num) (and (number? exp) (= exp num)))

; Sum is defined as a pair that starts with the symbol '+
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define addend cadr)
(define (augend x)
  (accumulate make-sum 0 (cddr x)))

(define (make-sum a1 a2) 
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list '+ a1 a2)]))

; Product stuff
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define multiplier cadr)
(define (multiplicand x)
  (accumulate make-product 1 (cddr x)))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))

; Exponent stuff
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)

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


; Find the derivative of some symbolic expression in lisp prefix notation,
; e.g. '(+ (* 2 x) y) with respect to some variable
(define (deriv exp var)
  (cond

    ; dc/dx = 0
    [(number? exp) 0]

    ; dx/dx = 1, dy/dx = 0
    [(variable? exp) (if (same-variable? exp var) 1 0)]

    ; d(a+b)/dx = da/dx + db/dx
    [(sum? exp) (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var))]
    
    ; d(ab)/dx = a*db/dx + b*da/dx
    [(product? exp)
      (make-sum
        (make-product (multiplier exp) (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var) (multiplicand exp)))]

    ; d(u^n)/dx = nu^(n-1)*du/dx
    [(exponentiation? exp)
      (make-product
        (make-product (exponent exp) (make-exponent (base exp) (make-sum (exponent exp) -1)))
        (deriv (base exp) var))]
    [else (error "unknown expression type: DERIV" exp)]))
  
(deriv '(* x y (+ x 3)) 'x)