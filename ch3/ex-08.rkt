#!/usr/bin/racket

#lang scheme

(define (make-f)
  (let [(state 1)]
    (lambda (op) 
      (display (list 'f op)) (newline)
        (if (eq? op 0)
          (set! state op)
          '())
        state)))

(define f (make-f))
(+ (f 0) (f 1))