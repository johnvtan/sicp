#!/usr/bin/racket

#lang sicp

(#%require "set-tree.rkt")

; Appends to list in left -> center -> right order
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

; Seems like both tree->list 1 and tree->list-2 always do left->center->right traversal
; and return the list in ascending order
(define t1 (list->set '(7 3 9 1 5 11)))
;t1
(tree->list-1 t1)
(tree->list-2 t1)

(define t2 (list->set '(3 7 5 9 11 1)))
;t2
(tree->list-1 t1)
(tree->list-2 t1)

(define t3 (list->set '(5 3 9 1 7 11)))
;t3
(tree->list-1 t3)
(tree->list-2 t3)