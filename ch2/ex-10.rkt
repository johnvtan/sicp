#!/usr/bin/racket
#lang sicp

(define (make-interval a b)
  (cons a b))

; Ex 2.7
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

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

(define (spans-zero x)
  (cond
    [(and (> (lower-bound x) 0) (> (upper-bound x) 0)) #f]
    [(and (< (lower-bound x) 0) (< (upper-bound x) 0)) #f]
    [else #t]))

(define (div-interval x y)
  (if (spans-zero y)
    (error "Dividend spans zero" y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y))))))

(define t1 (make-interval -3 3))
(define t2 (make-interval 4 5))
(define t3 (make-interval -3 0))
(define t4 (make-interval 0 3))

; OK
(div-interval t1 t2)
(div-interval t3 t2)
(div-interval t4 t2)

; NOT OK
;(div-interval t2 t1)
;(div-interval t2 t3)
(div-interval t2 t4)



                          