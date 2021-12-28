#!/usr/bin/racket

#lang sicp

(#%require "../common-ops.rkt")

; Selectors/data representation for expressions
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define  (=number? exp num) (and (number? exp) (= exp num)))

(define (contains? lst sym)
  (cond
    [(null? lst) #f]
    [(eq? (car lst) sym) #t]
    [else (contains? (cdr lst) sym)]))

(define (sum? x) (and (pair? x) (contains? x '+)))

; TODO this returns something like ((x), (3)) for the exp x+3
; need to either redefine how numbers/variables work, or unpack simple exprs like that
(define (split-by-+ x)
  (define (unpack-if-simple x)
    (if (= (length x) 1)
      (car x)
      x))
  (define (iter curr-sub-exp acc rest-of-exp)
    (cond
      [(null? rest-of-exp) (append acc (list curr-sub-exp))]
      [(eq? (car rest-of-exp) '+) (iter '() (append acc (list curr-sub-exp)) (cdr rest-of-exp))]
      [else (iter (append curr-sub-exp (list (car rest-of-exp))) acc (cdr rest-of-exp))]))
  (map unpack-if-simple (iter '() '() x)))

; Gets LHS of sum, which may be 
(define (addend x)
  (car (split-by-+ x)))

; Similar to ex-57, the augend has to be accumulated since it may
; contain many operands
(define (augend x)
  (let [(rhs-sub-exps (cdr (split-by-+ x)))]
    (if (null? rhs-sub-exps)
      (error "augend: exp not a sum" x)
      (accumulate make-sum 0 rhs-sub-exps))))

(define (make-sum a1 a2) 
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list a1 '+ a2)]))

; Product stuff
; Sums should just not be present here?
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define multiplier car)
(define (multiplicand x)
  (define (acc-func elem acc)
    (if (eq? elem '*)
      acc
      (make-product elem acc)))
  (accumulate acc-func 1 (cddr x)))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list m1 '* m2)]))

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

    [else (error "unknown expression type: DERIV" exp)]))
  
(deriv '(x * (x + 2 + x + 4)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3 * x * x + x) 'x)
(deriv '(3 * x * x * x + 2 * x + x * y) 'x)