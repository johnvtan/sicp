#!/usr/bin/racket

#lang sicp

(define (square-tree tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
            (square-tree sub-tree)
            (* sub-tree sub-tree)))
    tree))

(square-tree '(1 2 3 4 5))
(square-tree '(3))
(square-tree '())