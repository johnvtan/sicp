#!/usr/bin/racket

#lang scheme

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (random-in-range low high)
  (let [(range (- high low))]
    (+ low (* (random) range))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (let [(x-coord (random-in-range x1 x2))
          (y-coord (random-in-range y1 y2))]
      (pred x-coord y-coord)))
  (let [(area-pct (monte-carlo trials experiment))
        (area (abs (* (- y2 y1) (- x2 x1))))]
    (* area-pct area)))
    

(define (square x) (* x x))
(define (pred x y)
  (<= (+ (square x) (square y)) 1))

(exact->inexact (estimate-integral pred -1.0 1.0 -1.0 1.0 1000000))