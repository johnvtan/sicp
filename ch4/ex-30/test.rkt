#!/usr/bin/racket

#lang sicp 

(#%require "eval.rkt")
(#%require "environment.rkt")
(#%require "assert.rkt")

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
        (define (try a b) (if (= a 0) ((lambda () (- 5 4))) b))
        (try 0 (/ 1 0))))
  1)

; A -> Ben is right because display is a primitive procedure,
; so all arguments will be forced when the display expr is evaluated.
(eval-with-env
  '(begin
      (define (for-each proc items)
        (if (null? items)
          'done
          (begin (proc (car items))
                 (for-each proc (cdr items)))))
      (for-each (lambda (x) (display x) (newline))
        (list 57 321 88))))

; B
; without changes:
;  (p1 1) -> (1 2)
;  (p2 1) -> 1 : the set! call (e in p) is a thunk that doesn't get forced 
; with changes:
;  (p1 1) -> (1 2)
;  (p2 1) -> (1 2) : the set! call is now forced
(eval-with-env
  '(begin
    (define (p1 x)
      (set! x (cons x '(2)))
      x)
    (define (p2 x)
      (define (p e)
        e 
        x)
      (p (set! x (cons x '(2)))))
    (p1 1)))

; C
; Calling (actual-value) on some obj that isn't a thunk just returns that obj
; The arguments are forced already anyway since they are passed to the primitive display
; so calling actual-value on them again doesn't do anything.

; D
; Cy's approach is nice to ensure that side effects take place.
; presumably, sequences are written because you need their side effects so it would make
; sense for the language to force them to happen in the proper order.




(display "TESTS PASS") (newline)

; I don't feel like figuring out how to write a macro that catches exceptions
; so just uncomment these when you want to test

; (eval-with-env '(begin
;                   (define x 4)
;                   (+ y 3)))