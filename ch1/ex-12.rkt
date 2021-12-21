#!/usr/bin/racket
#lang sicp

; Prints out pascals triangle up to a depth of n
(define (pascals-triangle n)

  (define (first lst)
    (car lst))

  (define (second lst)
    (car (cdr lst)))

  (define (rest lst)
    (cdr lst))

  ; Given currlist, return a list which adds
  ; adjacent pairs in currlist
  (define (next-line currlist)

    (define (add-adj-pairs acc rest-of-currlist)
      ; (display acc) (display " ") (display rest-of-currlist) (newline)
      (if (= (length rest-of-currlist) 1)
        acc
        (add-adj-pairs
          (append acc (list (+ (first rest-of-currlist) (second rest-of-currlist))))
          (rest rest-of-currlist))))

    (append '(1) (add-adj-pairs '() currlist) '(1)))

  (define (inner currlist)
    (display currlist) (newline)
    (if (= (length currlist) n)
      (display "done\n")
      (inner (next-line currlist))))
  (inner (list 1)))

(pascals-triangle 10)