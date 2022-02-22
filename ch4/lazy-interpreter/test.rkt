#!/usr/bin/racket

#lang sicp 

(#%require "eval.rkt")
(#%require "environment.rkt")
(#%require "assert.rkt")
(#%require "time.rkt")

(define (eval-with-env expr)
  (actual-value expr (setup-environment)))

(define (eval-with-lazy-lists expr)
  (define list-op-defs
    '(begin
      (define (cons x y) (lambda (m) (m x y)))
      (define (car z) (z (lambda (p q) p)))
      (define (cdr z) (z (lambda (p q) q)))

      (define (list-ref items n)
        (if (= n 0)
          (car items)
          (list-ref (cdr items) (- n 1))))

      (define (map proc items)
        (if (null? items)
          '()
          (cons (proc (car items)) (map proc (cdr items)))))))
  (let [(env (setup-environment))]
    (myeval list-op-defs env)
    (actual-value expr env)))

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
        (define (try a b) (if (= a 0) ((lambda () (- 5 4))) b))
        (try 0 (/ 1 0))))
  1)

(assert-eq
  (eval-with-env
    '(begin
      (define x 5)
      (define x 23)
      x))
  23)

(assert-eq
  (eval-with-lazy-lists
    '(begin
      (define (add-lists list1 list2)
        (cond
          [(null? list1) list2]
          [(null? list2) list1]
          [else (cons (+ (car list1) (car list2))
                      (add-lists (cdr list1) (cdr list2)))]))
      (define ones (cons 1 ones))
      (define integers (cons 1 (add-lists ones integers)))
      (list-ref integers 17)))
  18)

(assert-eq
  (eval-with-lazy-lists
    '(begin
      (car (cdr '(1 2 3)))))
  2)


(display "TESTS PASS") (newline)

; I don't feel like figuring out how to write a macro that catches exceptions
; so just uncomment these when you want to test

; (eval-with-env '(begin
;                   (define x 4)
;                   (+ y 3)))