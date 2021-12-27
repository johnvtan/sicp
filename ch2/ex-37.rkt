#!/usr/bin/racket

#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (accumulate op init (map car seqs)))
          (accumulate-n op init (accumulate op init (map cdr seqs))))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector '((1 2 3) (1 2 3)) '(4 5 6))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((1 2 3) (1 2 3) (1 2 3)))

(define (matrix-*-matrix m n)
  (let [(cols (transpose n))]
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))

(matrix-*-matrix '((1 2) (3 4)) '((1 2) (3 4)))

