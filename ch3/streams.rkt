#!/usr/bin/racket

#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (list->stream lst)
  (if (null? lst)
    the-empty-stream
    (cons-stream (car lst) (list->stream (cdr lst)))))

(define (stream->list stream)
  (if (stream-null? stream)
    '()
    (cons (stream-car stream) (stream->list (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map proc (map stream-cdr argstreams)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred args)
  (cond
    [(stream-null? args) the-empty-stream]
    [(pred (stream-car args)) (cons-stream (stream-car args) (stream-filter pred (stream-cdr args)))]
    [else (stream-filter pred (stream-cdr args))]))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))
  
(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (stream-map + ones integers)))

(#%provide stream-car stream-cdr list->stream stream->list stream-map stream-enumerate-interval
  stream-filter stream-ref scale-stream ones integers)