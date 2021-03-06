#!/usr/bin/racket
#lang sicp

; simple way to test for primality - iterate through numbers from 2 until sqrt(n) and see if it
; divides evenly. If no integers in this range divide evenly, it is prime.
(define (prime-simple? n)
  (define (smallest-divisor-simple n)
    (define (divides? a b)
      (= (remainder b a ) 0))
    (define (find-divisor n test)
      (cond 
        [(> (* test test) n) n]
        [(divides? test n) test]
        [else (find-divisor n (+ test 1))]))
    (find-divisor n 2))
  (= (smallest-divisor-simple n) n))

(prime-simple? 3)
(prime-simple? 42)
(prime-simple? 71)

; if n is a prime number and a is any positive integer less than n, then a raised to the nth power
; is congruent to a mod n
; This means that (remainder a^n n) should equal a for any prime number 
(define (prime-fermat? n)
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

  (prime? n 100))

(prime-fermat? 3)
(prime-fermat? 42)
(prime-fermat? 71)
