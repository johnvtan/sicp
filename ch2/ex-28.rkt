#!/usr/bin/racket

#lang sicp

(define (reverse items)
  (define (iter acc items)
    (if (null? items)
      acc
      (iter (cons (car items) acc) (cdr items))))
  (iter '() items))

; Given a tree, return a list whose elements are
; all the leaves of the tree arragned in left-to-right
; order
(define (fringe tree)
  (define (iter acc tree)
    (cond
      ; end of list -> return acc  
      [(null? tree) acc]

      ; first elem is itself a list
      ; call iter on first elem and append to end of acc
      [(pair? (car tree))
        (iter (append acc (iter '() (car tree))) (cdr tree))]
      
      ; Otherwise, we can just add the first elem to the end of acc
      [else (iter (append acc (list (car tree))) (cdr tree))]))
  
  (iter '() tree))

(fringe '(1 2 3 4))
(fringe '(1 (2 3) 4))
(fringe '((1 2) (3 4) (5 (6 7))))

