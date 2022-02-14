#!/usr/bin/racket

#lang scheme

((lambda (n)
  ((lambda (fact) (fact fact n))

   ; a procedure that takes 2 arguments -- ft (itself) and k
   ; the trick here is giving a function **itself** as an argument
   ; which lets the function call itself without having definitions.
   (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
  10)

; fib proc
((lambda (n)
  ((lambda (fib) (fib fib n))
    
    (lambda (inner-fib k)
      (if (< k 2)
        1
        (+ (inner-fib inner-fib (- k 1)) (inner-fib inner-fib (- k 2)))))))
  9)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
    (lambda (ev? od? n)
      (if (= n 0) #t (od? ev? od? (- n 1))))
    (lambda (ev? od? n)
      (if (= n 0) #f (ev? ev? od? (- n 1))))))

(f 10)
(f 11)
(f 12)
