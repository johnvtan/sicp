#!/usr/bin/racket

#lang sicp 

(#%require "eval.rkt")
(#%require "environment.rkt")
(#%require "assert.rkt")

(define (eval-with-env expr)
  (myeval expr (setup-environment)
    (lambda (val fail) val)
    (lambda () (error "EVAL FAILED"))))

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

; amb test cases
(assert-eq
  (eval-with-env
    '(begin 
      (define (prime? n)
        (define (smallest-divisor-simple n)
          (define (divides? a b)
            (= (remainder b a) 0))
          (define (find-divisor n test)
            (cond 
              [(> (* test test) n) n]
              [(divides? test n) test]
              [else (find-divisor n (+ test 1))]))
          (find-divisor n 2))
        (= (smallest-divisor-simple n) n))

      (define (require p) (if (not p) (amb)))
      (define (an-element-of items)
        (require (not (null? items)))
        (amb (car items) (an-element-of (cdr items))))

      (define (prime-sum-pair list1 list2)
        (let [(a (an-element-of list1))
              (b (an-element-of list2))]
          (require (prime? (+ a b)))
          (list a b)))

      (prime-sum-pair '(5 1 3 8) '(20 35 110))))
  '(3 20))


(display "TESTS PASS") (newline)

; I don't feel like figuring out how to write a macro that catches exceptions
; so just uncomment these when you want to test

; (eval-with-env '(begin
;                   (define x 4)
;                   (+ y 3)))