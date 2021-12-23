#!/usr/bin/racket

#lang sicp

(define l1 '(1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '(1 (2 (3 (4 (5 (6 (7))))))))

; get 7 from each of the lists above
(car (cdr (car (cdr (cdr l1)))))
(car (car l2))

; where cadr -> (car (cdr x))
(car (cadr (cadr (cadr (cadr (cadr (cadr l3)))))))
