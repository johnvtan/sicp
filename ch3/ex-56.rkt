#!/usr/bin/racket

#lang sicp

(#%require "streams.rkt")

(define (merge s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
      (let [(s1car (stream-car s1))
            (s2car (stream-car s2))]
        (cond
          [(< s1car s2car)
            (cons-stream s1car (merge (stream-cdr s1) s2))]
          [(> s1car s2car)
            (cons-stream s2car (merge s1 (stream-cdr s2)))]
          [else
            (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))]))]))

; ???? magic
(define S
  (cons-stream 1
    (merge (scale-stream S 2) 
      (merge (scale-stream S 3) (scale-stream S 5)))))

(stream-ref S 10)

; This is powers of 2
; Is it true that the powers of 2 are all the integers whos only prime factor is 2?
; I guess so?
(define S2 (cons-stream 1 (scale-stream S2 2)))

; So this is all numbers whos only prime factors are 2 and 3
; (scale-stream S3 2) is not actually powers of 2 anymore, I guess we can't break
; down the definition so clearly because it's all defined in terms of itself.
; The base case of S3 is 1, (scale-stream S3 2)  is 2, and (scale-stream S3 3) is 3
; so the first 3 elems in S3 are (1, 2, 3). Then what?
; I guess another way to think about this is that there's no way any multiple of 5
; could appear in the S3. We start off with a base case of (1, 2, 3), and keep
; multiplying 2 or 3 to previous elements of the stream. Because 5 is a prime,
; there's no way multiplying any combination of 2 or 3 will result in a multiple of 5
; in the stream. The same is true for any other primes. Then in the above definition 
; of S, the 5 comes when we merge with (scale-stream S 5). But then there's still no
; way for higher primes to enter the stream.
; Still, I don't really fully understand the like state-by-state execution o this stream.
(define S3 (cons-stream 1 (merge (scale-stream S3 2) (scale-stream S3 3))))

