#!/usr/bin/racket

#lang sicp

(define (reverse items)
  (define (iter acc items)
    (if (null? items)
      acc
      (iter (cons (car items) acc) (cdr items))))
  (iter '() items))

(define (deep-reverse items)
  (cond
    [(list? items)
      ; call deep-reverse on each subtree
      (let [(reversed-children (map deep-reverse items))]
        ; then reverse the order at this layer
        (reverse reversed-children))]
    ; not a list -> not a subtree, so a single item
    ; return that item back since we can't really reverse it
    [else items]))

(deep-reverse '(1 2 3 4))
(deep-reverse '())
(deep-reverse '(1))
(deep-reverse '((1 2) (3 4)))
(deep-reverse '(1 (2 (3 (4 5)))))
