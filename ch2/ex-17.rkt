#!/usr/bin/racket

#lang sicp

(define (last-pair lst)
  (cond
    [(null? lst) (error "wtf bro")]
    [(null? (cdr lst)) lst]
    [else (last-pair (cdr lst))]))
 
(last-pair '(1 2 3 4))
(last-pair '(1))
(last-pair '())