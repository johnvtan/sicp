#!/usr/bin/racket
#lang sicp

(define (accumulate-filter condition combiner null-val term a next b)
  (define (iter a result)
    (cond
      [(> a b) result]
      [(condition a) (iter (next a) (combiner (term a) result))]
      [else (iter (next a) result)]))
  (iter a null-val))

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
  (if (= n 1)
    false
    (= (smallest-divisor-simple n) n)))

(define (inc n) (+ n 1))
(define (square n) (* n n))

(define (prime-sum-of-squares a b)
  (accumulate-filter prime? + 0 square a inc b))

(prime-sum-of-squares 1 10)

(define (euclid-gcd a b)
  (if (= b 0)
    a
    (euclid-gcd b (remainder a b))))

(define (ident x) x)

(define (relatively-prime-prod n)
  (define (condition x)
    (= (euclid-gcd n x) 1))
  (accumulate-filter condition * 1 ident 0 inc n))

(relatively-prime-prod 10)

 
