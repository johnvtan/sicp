#!/usr/bin/racket

#lang sicp 

(#%require "eval.rkt")
(#%require "environment.rkt")
(#%require "assert.rkt")
(#%require "time.rkt")

(define (eval-with-env expr)
  (actual-value expr (setup-environment)))

(assert-eq (eval-with-env '3) 3)
(assert-eq (eval-with-env '(+ 1 2)) 3)
(assert-eq (eval-with-env '(* 4 3)) 12)
(assert-eq (eval-with-env '(cons 1 2)) (cons 1 2))

; if statements
(assert-eq (eval-with-env '(if (eq? 3 4) 0 1)) 1)
(assert-eq (eval-with-env '(if (eq? 4 4) 0 1)) 0)
(assert-eq (eval-with-env '(if (if (eq? 3 4) true false) (+ 3 8) (* 4 2))) 8)
(assert-eq 
  (eval-with-env
    '((if (eq? 10 11) + -) 3 3))
  0)

; conds
(assert-eq
  (eval-with-env
    '(cond
       [(eq? 3 4) "no"]
       [(eq? 8 6) "no"]
       [(eq? (+ 3 3) 6) "yes"]
       [else "no"]))
  "yes")

(assert-eq
  (eval-with-env
    '(cond
       [(eq? 3 4) "no"]
       [(eq? 8 6) "no"]
       [(eq? (+ 3 3) 7) "no"]
       [else "yes"]))
  "yes")

; define + begin 
(assert-eq
  (eval-with-env
    '(begin
      (define x 3)
      (define y 8)
      (* x y)))
  24)

; lambda
(assert-eq
  (eval-with-env
    '((lambda (x) (+ x 2)) 5))
  7)

; assignment
(assert-eq
  (eval-with-env
    '(begin
      (define x 4)
      (set! x 23)
      (* x 2)))
  46)

; lambda with different scope 
(assert-eq
  (eval-with-env
    '(begin
      (define x 23)
      (define y 42)
      (define (proc x)
        (+ x y))
      (proc 100)))
  142)

; fact 
(assert-eq
  (eval-with-env
    '(begin
      (define (fact n)
        (if (eq? n 0)
          1
          (* n (fact (- n 1)))))
        (fact 5)))
  120)

; let's
(assert-eq
  (eval-with-env
    '(let [(x 3) (y 4)]
      (+ x y)))
  7)

(assert-eq
  (eval-with-env
    '(begin
      (define outer 42)
      (define x 32)
      (let [(x 1000)]
        (let [(y 23)]
          (+ outer x y)))))
  (+ 1000 23 42))

(assert-eq
  (eval-with-env
    '((lambda ()
        (define x 3)
        (+ x 45))))
  48)

(assert-eq
  (eval-with-env
    '(unless (= 4 3) 123 456))
  456)

(assert-eq
  (eval-with-env
    '(unless (> 4 3) 123 456))
  123)

; lazy test
(assert-eq
  (eval-with-env
    '(begin
        (define (try (a lazy) (b lazy)) (if (= a 0) ((lambda () (- 5 4))) b))
        (try 0 (/ 1 0))))
  1)

; 1. design a test for this
; probably reliant on set! to show delayed side effects
(assert-eq
  (eval-with-env
    '(begin
      (define a 0)
      (define b 0)
      (define c 0)
      (define d 0)
      (define (f a (b lazy) c (d lazy-memo))
        a b c d
        ; force it to be called by passing it to primitive
        ; (list a a b b b c c d d d)
        'done)
      (f (set! a (+ a 32)) (set! b (+ b 1)) (set! c (+ c 123)) (set! d (+ d 1)))
      (list a b c d)))

  ; a is forced on function enter once
  ; b is lazy and not forced 
  ; c is forced on function enter once
  ; d is lazy and not forced 
  (list 32 0 123 0))

(assert-eq
  (eval-with-env
    '(begin
      (define a 0)
      (define b 0)
      (define c 0)
      (define d 0)
      (define (f a (b lazy) c (d lazy-memo))
        ; force it to be called by passing it to primitive
        (list a a b b b c c d d d)
        'done)
      (f (set! a (+ a 32)) (set! b (+ b 1)) (set! c (+ c 123)) (set! d (+ d 1)))
      (list a b c d)))

  ; a is forced on function enter once
  ; b is lazy and not memoized so called 3 times 
  ; c is forced on function enter once
  ; d is lazy and memoized so only called once 
  (list 32 3 123 1))


(display "TESTS PASS") (newline)

; I don't feel like figuring out how to write a macro that catches exceptions
; so just uncomment these when you want to test

; (eval-with-env '(begin
;                   (define x 4)
;                   (+ y 3)))