#!/usr/bin/racket

#lang sicp

(#%require "set-tree.rkt")

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

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

(define (union-set s1 s2)
  (define (helper s1 s2)
    (cond
      [(null? s1) s2]
      [(null? s2) s1]
      [(= (car s1) (car s2))
        (cons (car s1) (helper (cdr s1) (cdr s2)))]
      [(< (car s1) (car s2))
        (cons (car s1) (helper (cdr s1) s2))]
      [else
      (cons (car s2) (helper s1 (cdr s2)))]))
  
  (list->tree (helper (tree->list s1) (tree->list s2))))

(define (intersection-set s1 s2)
  (define (helper s1 s2)
    (cond
      [(or (null? s1) (null? s2)) '()]
      [(= (car s1) (car s2))
        (cons (car s1) (helper (cdr s1) (cdr s2)))]
      [(< (car s1) (car s2)) (helper (cdr s1) s2)]
      [else (helper s1 (cdr s2))]))
  
  (list->tree (helper (tree->list s1) (tree->list s2))))

(define s1 (list->tree '(1 2 3 4 5)))
(define s2 (list->tree '(3 4 5 6 7)))

(union-set s1 s2)
(tree->list (union-set s1 s2))

(intersection-set s1 s2)
(tree->list (intersection-set s1 s2))