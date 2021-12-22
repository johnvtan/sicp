#!/usr/bin/racket

#lang sicp

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
      a
      (gcd b (remainder a b))))
  

  ; seems like remainder might do this for us?
  ; use abs to get this to work better?
  (let [(div (abs (gcd n d)))
        (sign (if (< d 0) -1 1))]
    (cons (* sign (/ n div)) (* sign (/ d div)))))

; Returns the numerator of a rational number
(define (numer rat)
  (car rat))

; Returns the denominator of a rational number
(define (denom rat)
  (cdr rat))
  
(define (print-rat r)
  (display (numer r)) (display "/") (display (denom r)))

(print-rat (make-rat -4 -6)) (newline)
(print-rat (make-rat -4 6)) (newline)
(print-rat (make-rat 4 6)) (newline)
(print-rat (make-rat 4 -6)) (newline)



