#!/usr/bin/racket
#lang sicp

; Point helpers
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(") 
  (display (x-point p)) 
  (display ", ") 
  (display (y-point p)) 
  (display ")"))

; Line segment stuff
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg)) (x-point (end-segment seg)))
              (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

; Higher level funcs that don't change
(define (rect-area r)
  (* (rect-height r) (rect-width r)))

(define (rect-perim r)
  (+ (* 2 (rect-height r)) (* 2 (rect-width r))))

; Rectangle stuff (first impl)
; (define (make-rect p1 p2)
;   (cons p1 p2))
; 
; (define (rect-p1 r)
;   (car r))
; 
; (define (rect-p2 r)
;   (cdr r))
; 
; (define (rect-height r)
;   (let [(p1 (rect-p1 r))
;         (p2 (rect-p2 r))]
;     (abs (- (y-point p1) (y-point p2)))))
; 
; (define (rect-width r)
;   (let [(p1 (rect-p1 r))
;         (p2 (rect-p2 r))]
;     (abs (- (x-point p1) (x-point p2)))))
; 
; (define r1 (make-rect (make-point 0 0) (make-point 4 10)))
; (rect-area r1)
; (rect-perim r1)
; 
; (define r2 (make-rect (make-point 4 10) (make-point -10 -10)))
; (rect-area r2)
; (rect-perim r2)

; Rect2 - need to comment out other impl for this to work, otherwise
; get redefinition error
; I wonder if there's a way to work around that?

; height and width are always positive
; any positioning must be done by changing the bottom-left anchor point
(define (make-rect bottom-left height width)
  (cons bottom-left (cons (abs height) (abs width))))

(define (rect-height r)
  (car (cdr r)))

(define (rect-width r)
  (cdr (cdr r)))

(define r1 (make-rect (make-point 0 0) 10 12))
(rect-area r1)
(rect-perim r1)

