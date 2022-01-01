#!/usr/bin/racket

#lang sicp

(#%require "huffman-tree.rkt")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (contains? elem lst)
    (not (eq? (memq elem lst) #f)))

  (cond
    ; I have done something horribly wrong if this executes
    [(null? tree) (error "SYMBOL NOT FOUND IN TREE" sym)]
    
    ; handle cases where left/right are leaves AND the symbol matches that leaf
    [(and (leaf? (right-branch tree)) (eq? sym (symbol-leaf (right-branch tree))))
      '(1)]
    [(and (leaf? (left-branch tree)) (eq? sym (symbol-leaf (left-branch tree))))
      '(0)]

    ; handle cases where left/right are not leaves AND the symbol is in the symbol list
    [(and (not (leaf? (right-branch tree))) (contains? sym (symbols (right-branch tree))))
      (cons 1 (encode-symbol sym (right-branch tree)))]
    [(and (not (leaf? (left-branch tree))) (contains? sym (symbols (left-branch tree))))
      (cons 0 (encode-symbol sym (left-branch tree)))]

    ; all other cases suck
    [else (error "I THINK THIS IS ERROR!")]))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
sample-message
(encode (decode sample-message sample-tree) sample-tree)