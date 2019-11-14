#!/usr/bin/racket
#lang sicp

(define (square n)
  (* n n))

; defines an exponent of a number modulo another number. like a normal exponent, but numbers wrap
; around after m steps.
(define (expmod base exponent m)
  (cond
    [(= exponent 0) 1]
    [(even? exponent)
      (remainder (square (expmod base (/ exponent 2) m)) m)]
    [else 
      (remainder (* base (expmod base (- exponent 1) m)) m)]))

; the fermat test checks if a^n mod n is equal to a
; and we pick some random value for a that is less than n
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; hard code the number of times we try. this is a probablisitc algorithm
(define (prime? n times)
  (cond
    [(= times 0) true]
    [(fermat-test n) (prime? n (- times 1))]
    [else false]))

(define (prime-fermat? n)
  (prime? n 100))

(prime-fermat? 561)
