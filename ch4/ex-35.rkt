#!/usr/bin/racket

#lang sicp

(#%require "amb/eval.rkt")
(#%require "amb/environment.rkt")

(define prog 
  '(begin

    (define (require p) (if (not p) (amb)))

    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))

    (define (range->list low high)
      (if (>= low high)
        '()
        (cons low (range->list (+ 1 low) high))))

    (define (an-integer-between low high)
      (an-element-of (range->list low high)))

    (define (a-pythagorean-triple-between low high)
      (let [(i (an-integer-between low high))]
        (let [(j (an-integer-between i high))]
          (let [(k (an-integer-between j high))]
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))
    
    (a-pythagorean-triple-between 1 30)))

(myeval prog (setup-environment)
  (lambda (value alt)
    (display value) (newline)
    (alt))
  (lambda () 'done))
