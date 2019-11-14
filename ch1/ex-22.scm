#!/usr/bin/racket
#lang sicp

(define (prime? n)
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

(define (timed-prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test n (runtime))) ; (runtime) returns the system's time

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))
    false))

(define (report-prime n elapsed-time)
  (newline)
  (display " *** ")
  (display "Prime found: ")
  (display n)
  (display " at time: ")
  (display elapsed-time)
  (newline)
  true)

; searches for primes in consecutive odd integers in a specified range 
(define (search-for-primes n n-primes)
  (if (> n-primes 0)
    (if (equal? (timed-prime-test n) true)
      (search-for-primes (+ n 2) (- n-primes 1))
      (search-for-primes (+ n 2) n-primes))))

(search-for-primes 1001 3)
(search-for-primes 10001 3)
(search-for-primes 100001 3)
(search-for-primes 1000001 3)
(search-for-primes 10000001 3)
(search-for-primes 1000000001 3)
