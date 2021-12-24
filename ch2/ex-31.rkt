#!/usr/bin/racket

#lang sicp

(define (tree-map fn tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
            (tree-map fn sub-tree)
            (fn sub-tree)))
    tree))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(square-tree '(1 2 3 4 5))