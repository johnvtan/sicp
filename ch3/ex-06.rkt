#!/usr/bin/racket

#lang scheme

(define (rand op)
  (cond
    [(eq? op 'generate) (random)]
    [(eq? op 'reset)
      (lambda (seed)
        (random-seed seed))]
    [else (error "BAD OP" op)]))

((rand 'reset) 1234)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1234)
(rand 'generate)
(rand 'generate)