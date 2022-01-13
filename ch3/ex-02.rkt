#!/usr/bin/racket

#lang scheme

(define (make-monitored f)
  (let [(num-calls 0)]
    (lambda (op)
      (cond
        [(eq? op 'how-many-calls?) num-calls]
        [(eq? op 'reset-count) (set! num-calls 0)]
        [else
          (set! num-calls (+ num-calls 1))
          (f op)]))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 196)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
