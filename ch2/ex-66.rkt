#!/usr/bin/racket

#lang sicp

(define (make-record key value)
  (cons key value))

(define key car)
(define value cdr)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(= (key x) (key (entry set))) #t]
    [(< (key x) (key (entry set))) (element-of-set? x (left-branch set))]
    [(> (key x) (key (entry set))) (element-of-set? x (right-branch set))]))

; Could be generic BST insert
(define (adjoin-set x set)
  (cond
    [(null? set) (make-tree x '() '())]
    [(= (key x) (key (entry set))) set]
    [(< (key x) (key (entry set)))
      (make-tree (entry set)
                 (adjoin-set x (left-branch set))
                 (right-branch set))]
    [(> (key x) (key (entry set)))
      (make-tree (entry set)
                 (left-branch set)
                 (adjoin-set x (right-branch set)))]))

(define (list->set lst)
  (define (iter lst tree)
    (if (null? lst)
      tree
      (iter (cdr lst) (adjoin-set (car lst) tree))))
  (iter lst '()))

(define (lookup given-key set-of-records)
  (cond
    [(null? set-of-records) #f]
    [(= given-key (key (entry set-of-records))) (entry set-of-records)]
    [(< given-key (key (entry set-of-records)))
      (lookup given-key (left-branch set-of-records))]
    [else (lookup given-key (right-branch set-of-records))]))
  
(define reclist (list (make-record 7 "seven") (make-record 3 "three") (make-record 5 "five")
                      (make-record 9 "nine") (make-record 11 "eleven")))

(define set-of-records (list->set reclist))
(lookup 7 set-of-records)
(lookup 11 set-of-records)
(lookup 45 set-of-records)
