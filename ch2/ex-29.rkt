#!/usr/bin/racket

#lang sicp

; A mobile consists of a left and right branch
; each of which has either a weight or another binary mobile
(define (make-mobile left right)
  (list left right))

; A binary mobile consists of a rod of some length together
; with a structure, which is either a single weight (number)
; or another mobile
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight b)
  (let [(structure (branch-structure b))]
    (cond
      [(number? structure) structure]
      [else (total-weight structure)])))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define b1 (make-branch 1 7))
(define b2 (make-branch 2 7))
(define m1 (make-mobile b1 b2))

(total-weight m1)

(define b3 (make-branch 3 14))
(define b4 (make-branch 3 m1))
(define m2 (make-mobile b3 b4))

(total-weight m2)
(newline)

(define (mobile-balanced? mobile)
  ; Returns the weight of the mobile if balanced,
  ; #f otherwise
  (define (mobile-weight-or-false-if-unbalanced m)
    (let [(left-weight-or-false (branch-weight-or-false-if-unbalanced (left-branch m)))
          (right-weight-or-false (branch-weight-or-false-if-unbalanced (right-branch m)))]
      (if (and
            ; both left and right are not false
            (number? left-weight-or-false)
            (number? right-weight-or-false)
            ; and torques are balanced
            (= (* left-weight-or-false (branch-length (left-branch m)))
               (* right-weight-or-false (branch-length (right-branch m)))))
        ; return the weight of this mobile
        (+ left-weight-or-false right-weight-or-false)
        #f)))

  ; Returns the weight of the branch if balanced,
  ; #f otherwise
  (define (branch-weight-or-false-if-unbalanced b)
    (let [(structure (branch-structure b))]
      (cond
        ; weight, not mobile at the end which is balanced by default
        [(number? structure) structure]
        [else (mobile-weight-or-false-if-unbalanced structure)])))
  
  (number? (mobile-weight-or-false-if-unbalanced mobile)))

; same weight, different length
(mobile-balanced? m1)

; same weight, same length, sub mobile unbalanced
(mobile-balanced? m2)
(newline)

(define m3 (make-mobile (make-branch 12 7) (make-branch 12 7)))
(define m4 (make-mobile b3 (make-branch (branch-length b3) m3)))

; same weight, same length
(mobile-balanced? m3)

; same weight, same length with balanced subtree
(mobile-balanced? m4)
(newline)

(define m5 (make-mobile (make-branch 12 7) (make-branch 12 6)))
(define m6 (make-mobile b3 (make-branch (branch-length b3) m5)))

; different weight, same length
(mobile-balanced? m5)

; different weight, same length, unbalanced subtree
(mobile-balanced? m6)

; Part D -> if we use cons instead of list, I probably just need
; to use cdr instead of cadr for getting whichever element is second
; in the pair (structure, right-branch)