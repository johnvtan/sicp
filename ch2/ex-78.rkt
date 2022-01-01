#!/usr/bin/racket

#lang scheme

; Global op-table + put/get
(define op-table (make-hash))

(define (put op type item)
  (hash-set! op-table (cons op type) item))

(define (get op type)
  (hash-ref op-table (cons op type)))

; tags
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    [(pair? datum) (car datum)]
    [(number? datum) 'scheme-number]
    [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (apply-generic op . args)
  (let [(type-tags (map type-tag args))]
    (let [(proc (get op type-tags))]
      (if proc
        (apply proc (map contents args))
        (error "No method for theset types: APPLY-GENERIC" (list op type-tags))))))
      
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'make 'scheme-number (lambda (x) x))
  'done)

(install-scheme-number-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(add 3 2)
(sub 3 2)
(mul 3 2)
(div 6 2)