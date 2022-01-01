#!/usr/bin/racket
#lang sicp

(#%require "huffman-tree.rkt")

(define freqs '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 16)
                (WAH 1) (YIP 9)))

(define message 
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(define rock-tree (generate-huffman-tree freqs))

(length (encode message rock-tree))

; 3 bits per symbol in fixed length
(* 3 (length message))
