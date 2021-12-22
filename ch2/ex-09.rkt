#!/usr/bin/racket
#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let [(p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (upper-bound x) (lower-bound y)))
        (p3 (* (lower-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; The width of some interval t3, where t3 = t1 + t2
; is the same as (width t2) + (width t1)
; Because (width t3) = ((upper t3) + (lower t3)) / 2
; and (upper t3) = (upper t1) + (upper t2)
; and (lower t3) = (lower t1) + (lower t2)
; therefore (width t3) = (((upper t1) + (upper t2)) + (lower t1 + lower t2)) / 2
; and the rhs of the above is the same as (width t1) + (width t2)
(define t1 (make-interval 4 6))
(define t2 (make-interval 3 7))
(width t1)
(width t2)
(width (add-interval t1 t2))

(newline)

; If it's true that the width of a multiplied interval is dependent only on widths,
; then multiplying t3 and t4 (which have the same widths as t1 and t2) should result
; in an interval with the same width as multiplying t1 and t2.
; But this is not the case
(define t3 (make-interval -1 1))
(define t4 (make-interval -2 2))
(= (width t1) (width t3))
(= (width t2) (width t4))
(= (width (mul-interval t1 t2)) (width (mul-interval t3 t4)))