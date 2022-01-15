#!/usr/bin/racket

#lang sicp

(define (make-queue)
  (let [(head '())
        (tail '())]
    
    (define (empty?)
      (null? head))
    
    (define (insert item)
      (let [(new-pair (cons item '()))]
        (cond
          [(empty?)
            (set! head new-pair)
            (set! tail new-pair)
            head]
          [else
            (set-cdr! tail new-pair)
            (set! tail new-pair)
            head])))
    
    (define (front)
      (if (empty?)
        (error "FRONT called on empty queue" head)
        (car head)))
    
    (define (delete)
      (if (empty?)
        (error "DELETE called on empty queue" head)
        (begin
          (set! head (cdr head))
          head)))

    (define (dispatch op)
      (cond
        [(eq? op 'insert) insert]
        [(eq? op 'delete) delete]
        [(eq? op 'front) front]
        [(eq? op 'empty?) empty?]))
    dispatch))

(define (insert-queue! q item) ((q 'insert) item))
(define (delete-queue! q) ((q 'delete)))
(define (empty-queue? q) ((q 'empty?)))
(define (front-queue q) ((q 'front)))

(define q (make-queue))
(empty-queue? q)
(insert-queue! q 'a)
(insert-queue! q 'b)
(front-queue q)
(empty-queue? q)

(delete-queue! q)
(delete-queue! q)
(empty-queue? q)