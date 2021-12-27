#!/usr/bin/racket

#lang sicp

(#%require sicp-pict)

(define (my-flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-cc-180 painter)
  ; just flip vertically and horizontally?
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-cc-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
                     
(paint (my-flip-horiz einstein))
(paint (rotate-cc-180 einstein))
(paint (rotate-cc-270 einstein))