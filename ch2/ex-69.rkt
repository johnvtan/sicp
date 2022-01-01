#!/usr/bin/racket

#lang sicp

(#%require "huffman-tree.rkt")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
    ; Terminate when we've merged everything into a single node 
    (car leaf-set)
    (let [(first-node (car leaf-set))
          (second-node (cadr leaf-set))
          (rest-of-leaf-set (cddr leaf-set))]
      ; merge the first two nodes, then add them back into the set
      (successive-merge 
        (adjoin-set (make-code-tree first-node second-node) rest-of-leaf-set)))))
  
(define freq-pairs '((A 4) (B 2) (C 1) (D 1)))
(define generated-tree (generate-huffman-tree freq-pairs))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message generated-tree)
(decode sample-message sample-tree)
