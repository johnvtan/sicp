#!/usr/bin/racket

#lang sicp 

(define (has-cycle? x)
  (define (contains? lst x)
    (cond
      [(null? lst) #f]
      [(eq? (car lst) x) #t]
      [else (contains? (cdr lst) x)]))
  
  (define seen '())
  (define (iter x seen)
    (display (list 'iter x seen)) (newline)
    (cond
      [(not (pair? x)) #f]
      [(contains? seen x) #t]
      [else (iter (cdr x) (cons x seen))]))
(iter x '()))

(has-cycle? '(a b c d))

(define z '(a b c))
(set-cdr! (cddr z) z)
(has-cycle? z)




