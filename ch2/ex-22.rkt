#!/usr/bin/racket

#lang sicp

(define (square x) (* x x))

(define (square-list items)

  ; This procedure returns the answers in reverse because answer starts out as nil,
  ; which is the end of the list. Then we cons the square of the first item to the list
  ; meaning that the square of the first item will be at the end of the list.
  ; Then on the next iteration, we cons the square of the second item to that list,
  ; meaning the square of the second item will be ahead of the square of the first item.
  (define (iter things answer)
    (if (null? things)
      answer
      ; Reversing the order here (i.e. calling (cons answer (square (car things))))
      ; doesn't work because of the definition of a list, which ends with a '().
      ; If we do the above, then after the first iteration, answer will look like
      ; '() . (square (car things))
      ; which isn't a list, because '() needs to come at the end of the list.
      (iter (cdr things) (cons (square (car things)) answer))))
  (iter items '()))


(square-list '(1 2 3 4 5))