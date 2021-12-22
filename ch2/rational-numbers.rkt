#!/usr/bin/racket

#lang sicp

; Creates a rational number given its numerator and denominator
; Simplifies the fraction
(define (make-rat numer denom)
  (define (gcd a b)
    (if (= b 0)
      a
      (gcd b (remainder a b))))
    
  (let ((div (gcd numer denom)))
    (cons (/ numer div) (/ denom div))))

; Returns the numerator of a rational number
(define (numer rat)
  (car rat))

; Returns the denominator of a rational number
(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)
     (* (denom x) (numer y)))))

(define (print-rat r)
  (display (numer r)) (display "/") (display (denom r)))

(print-rat (make-rat 2 3)) (newline)
(print-rat (make-rat 10 15)) (newline)

(let ((x (make-rat 2 3)) (y (make-rat 4 5)))
  (print-rat (add-rat x y)) (newline))