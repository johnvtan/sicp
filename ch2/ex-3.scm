#!/usr/bin/racket
#lang sicp

(#%require "ex-2.scm")

; rectangle from 2 points - stores the top right and bottom left points
(define (make-rect1 p1 p2)
  (cons 
    (make-point
      (min (x-point p1) (x-point p2))
      (min (y-point p1) (y-point p2)))
    (make-point
      (max (x-point p1) (x-point p2))
      (max (y-point p1) (y-point p2)))))

(define (rect1-btm-left rect)
  (car rect))

(define (rect1-top-right rect)
  (cdr rect))

(define (rect1-height rect)
  (- (y-point (rect1-top-right rect))
     (y-point (rect1-btm-left rect))))

(define (rect1-width rect)
  (- (x-point (rect1-top-right rect))
     (x-point (rect1-btm-left rect))))

(define (rect-area rect)
  (* (rect1-height rect)
     (rect1-width rect)))

(define (rect-perim rect)
  (+ (* 2 (rect1-height rect))
     (* 2 (rect1-width rect))))

(define r (make-rect1 (make-point 3 2) (make-point 1 4)))
(rect-area r)
(rect-perim r)

; for the second rectangle type which i won't implement, i'd store it as origin, height, width and
; have rect-height/rect-width get the height/width directly
