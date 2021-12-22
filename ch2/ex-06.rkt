#!/usr/bin/racket

#lang sicp

; Zero is defined as a procedure that, given a procedure,
; returns a different procedure that returns its argument
; I guess, in general, numbers in this fucked up form
; are procedures that take other procedures as arguments
; and return procedures
; Also note that zero applies f to x 0 times
(define zero (lambda (f) (lambda (x) x)))

; In the case where we do (add-1 zero),
; this becomes:
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda x) x))))
; (lambda (f) (lambda (x) (f x)))
; So 1 is just (lambda (x) (f x))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; one applies f to x 1 time
(define one (lambda (f) (lambda (x) (f x))))

; two applies f to x 2 times
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (to-real-num n)
  (define (inc x)
    (+ x 1))

  ; (n inc) returns a procedure that takes in some value
  ; and applies inc to that value n times
  ; So to recover an actual number out of a church numeral,
  ; we define inc to count the number of times the function
  ; has been chained 
  ((n inc) 0))

(to-real-num zero)
(to-real-num (add-1 zero))
(to-real-num (add-1 (add-1 two)))

; To add, evaluate ((b f) x), which is b repeated applications of f
; on x, then pass the result to (a f), which calls f a more times on
; that result
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(newline)
(to-real-num (add zero two))
(to-real-num (add two zero))
(to-real-num (add two one))
(to-real-num (add one one))
(to-real-num (add one two))
(to-real-num (add two two))

; To multiply, we do (a (b f)) which calls (b f) a times
(define (mul a b)
  (lambda (f) (lambda (x) ((a (b f)) x))))

(newline)
(to-real-num (mul two zero))
(to-real-num (mul three two))