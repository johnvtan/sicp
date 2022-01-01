#!/usr/bin/racket

#lang sicp

(#%provide
  make-leaf leaf? symbol-leaf weight-leaf
  make-code-tree left-branch right-branch symbols weight decode choose-branch
  adjoin-set make-leaf-set
  generate-huffman-tree
  encode)

; some dynamically typed nodes
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; I wonder why they decided to have different representations for the leaf
; and other nodes
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let [(next-branch (choose-branch (car bits) current-branch))]
        (if (leaf? next-branch)
          ; is leaf -> use symbol at this leaf, restart tree traversal at next bit
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          ; not leaf -> continue traversal for this symbol on next branch
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    [(= bit 0) (left-branch branch)]
    [(= bit 1) (right-branch branch)]
    [else (error "bad bit: CHOOSE-BRANCH" bit)]))

; Is this set separate from the make-code-tree above?
; What is this used for?
(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

; Orders a set of pairs so that it's ready to be merged
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let [(pair (car pairs))]
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

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
