#!/usr/bin/racket

#lang sicp

(define (make-frame orig e1 e2)
  (list orig e1 e2))

; orig -> car
; e1 -> cadr
; e2 -> caddr

(define (make-frame2 orig e1 e2)
  (cons orig (cons e1 e2)))

; orig -> car
; e1 -> cadr
; e2 -> cddr
