#!/usr/bin/racket

#lang sicp

(#%require sicp-pict)

(define (below2 p1 p2)
  (let [(split-point (make-vect 0 0.5))]
    (let [(paint-top (transform-painter p1
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        split-point))
          (paint-bottom (transform-painter p2
                          split-point
                          (make-vect 1.0 0.5)
                          (make-vect 0.0 1.0)))]
        (lambda (frame)
          (paint-top frame)
          (paint-bottom frame)))))

(define (below3 p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))

(paint (below2 einstein gray))
(paint (below3 einstein gray))