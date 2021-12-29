#!/usr/bin/racket

#lang sicp

(#%require "set-tree.rkt")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; Does this return a binary tree?
(define (partial-tree elts n)
  ;(display elts) (display " ") (display n) (newline)
  (if (= n 0)
    ; Base case -> no elements allocated to this partial-tree,
    ; return '() as the tree and the entire list of elts
    (cons '() elts)

    ; Otherwise, this partial-tree should contain some number of elements
    ; So we can split it up between the left and right trees
    ; quotient is integer division, roudning down
    ; I think this means that the tree will have more elements on the right subtree
    ; if n is odd 
    ; we subtract 1 because we need to use one element on this entry
    (let [(left-size (quotient (- n 1) 2))]
      ; Recursively call partial-tree with all the elements 
      (let [(left-result (partial-tree elts left-size))]
        ; Retrieve the results of the above recursive call
        (let [(left-tree (car left-result))
              (non-left-elts (cdr left-result))
              ; Calculate the right side, which gets everything not used by the left side
              ; Excluding the element used for this-entry
              (right-size (- n (+ left-size 1)))]
          
          ; Now take the element for the current node
          ; If elts is sorted, then it should hold that all elements in the left subtree
          ; are < this-entry, and everything in the right sub-tree is > this-entry
          (let [(this-entry (car non-left-elts))
                ; And make the right subtree with the rest of the elts
                (right-result (partial-tree (cdr non-left-elts) right-size))]
            (let [(right-tree (car right-result))
                  (remaining-elts (cdr right-result))]
              ; Finally, make the actual tree using the left and right subtrees
              ; assuming n == len(elts), remaining-elts by this point should be '()
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
  
; I think list->tree is O(n)
; it needs to examine each element in the list exactly once to construct
; a balanced tree 

(list->tree '(1 3 5 7 9 11 13))

; Note that this doesn't return a BST
(list->tree '(11 9 7 5 3 1))

