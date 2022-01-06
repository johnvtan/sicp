#!/usr/bin/racket

#lang scheme 

; Implement a dense polynomial representation
; This means that the order of the coefficient is implied by its position
; in the term list. No explicit value for "order" exists in the term list.
;
; We would like the representation to be generic over the add, mul, =zero?, sub
; operators, which mostly use first-term and rest-terms to operate on the termlist.
; first-term should return something that contains both the coefficient (which is
; stored in the dense termlist) and the order (which is not explicitly stored in
; the dense termlist).

; Dense polynomials contain a member that stores the length of the term-list
; which will help us with getting the order for first-term
; term-list here is assumed to be of the form '(c(n-1) c(n-2) ... c0), where cn
; is the nth order coefficient.
(define (make-poly variable term-list) 
  (cons variable term-list))

(define (variable p) (car p))

; Which is now of the form (len term-list)
(define (term-list p) (cdr p))

(define (make-term-list order t) (list order t))
(define (term-list-order t) (car t))
(define (term-list-coeffs t) (cadr t))

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (empty-termlist? t) (= -1 (order t)))
(define (the-empty-termlist) (make-term-list -1 '()))

(define (first-term term-list) 
  (make-term (term-list-order term-list) (car (term-list-coeffs term-list))))

(define (rest-terms term-list) 
  (list (- (term-list-order term-list) 1) (cdr (term-list-coeffs term-list))))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (adjoin-term term term-list)
  (define (fill-out-term-list-to-nth-order n t)
    (if (= n (term-list-order t))
      t
      (fill-out-term-list-to-nth-order n 
        (make-term-list (+ (term-list-order t) 1)
                        (cons 0 (term-list-coeffs t))))))
  (cond 
    ; TODO replace with =zero? later
    [(= (coeff term) 0) term-list]

    ; TODO allow adjoing if coeff in term-list is 0?
    [(<= (order term) (term-list-order term-list))
      (error "Cannot adjoin term to term-list since the order already exists" term term-list)]
    [else 
      (let [(new-term-list (fill-out-term-list-to-nth-order (- (order term) 1) term-list))]
        (make-term-list (order term) (cons (coeff term) (term-list-coeffs new-term-list))))]))

(define t (adjoin-term (make-term 4 2) (adjoin-term (make-term 2 7) (the-empty-termlist))))
(define t2 (adjoin-term (make-term 5 6) t))

(make-poly 'x t2)
(first-term t2)
(first-term (rest-terms t2))
(first-term (rest-terms (rest-terms t2)))
(first-term (rest-terms (rest-terms (rest-terms t2))))
(first-term (rest-terms (rest-terms (rest-terms (rest-terms t2)))))
(first-term (rest-terms (rest-terms (rest-terms (rest-terms (rest-terms t2))))))