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
    
(define x (list->stream '(1 2 3 4)))
(stream->list (stream-map + (list->stream '(1 2 3 4)) (list->stream '(5 6 7 8))))
