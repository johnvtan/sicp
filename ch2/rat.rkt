#!/usr/bin/racket
#lang sicp

(#%provide (all-defined))

; ex 1 - handle negatives properly
(define (make-rat n d)
  (define div (gcd n d))
  (cond
    [(< d 0)
      (cons (/ (* -1 n) div) (/ (* -1 d) div))]
    [else
      (cons (/ n div) (/ d div))])) 

(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (print-rat rat)
  (display (numer rat))
  (cond
    [(not (= (denom rat) 1))
      (display "/")
      (display (denom rat))])
  (newline))

(define (add-rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2))
               (* (denom r1) (numer r2)))
            (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (make-rat (- (* (numer r1) (denom r2))
               (* (denom r1) (numer r2)))
            (* (denom r1) (denom r2))))

(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(define (div-rat r1 r2)
  (make-rat (* (numer r1) (denom r2))
            (* (denom r1) (numer r2))))

(define (equal-rat? r1 r2)
  (= (* (numer r1) (denom r2))
     (* (denom r1) (numer r2))))

