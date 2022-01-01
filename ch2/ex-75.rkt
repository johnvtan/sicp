#!/usr/bin/racket

#lang scheme
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))


(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part) (* r (cos a))]
      [(eq? op 'imag-part) (* r (sin a))]
      [(eq? op 'magnitude) r]
      [(eq? op 'angle) a]
      [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)

(define z (make-from-mag-ang 8.0623 2.0899))
(z 'real-part)
(z 'imag-part)
