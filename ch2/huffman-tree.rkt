#!/usr/bin/racket

#lang sicp

(#%provide
  make-leaf leaf? symbol-leaf weight-leaf
  make-code-tree left-branch right-branch symbols weight decode choose-branch
  adjoin-set make-leaf-set)

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
