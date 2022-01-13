#!/usr/bin/racket

#lang sicp

(define (count-pairs x)
  (define (contains? lst x)
    (cond
      [(null? lst) #f]
      [(eq? (car lst) x) #t]
      [else (contains? (cdr lst) x)]))
  
  (define seen '())
  (define (iter x)
    (display (list 'iter x seen)) (newline)
    (cond
      [(not (pair? x)) 0]
      [(contains? seen x) 0]
      [else
        (set! seen (cons x seen))
        (+ (iter (car x))
            (iter (cdr x))
            1)]))
(iter x))

(count-pairs '(a b c))
(define y '(a))
(define lst (cons y y))
(count-pairs (list lst))
(count-pairs (cons lst lst))

(define z '(a b c))
(set-cdr! (cddr z) z)
(count-pairs z) ; never returns


