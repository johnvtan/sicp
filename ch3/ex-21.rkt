#!/usr/bin/racket

#lang sicp 

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))
  
(define (insert-queue! queue item)
  (let [(new-pair (cons item '()))]
    (cond
      [(empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue]
      [else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue])))

(define (delete-queue! queue)
  (cond
    [(empty-queue? queue) (error "DELETE! called with an empty queue" queue)]
    [else
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue]))

(define (print-queue queue)
  ; the state of the queue is really represented by the front-ptr
  ; rear-ptr is just for speeding up inserts, and isn't properly updated on deletes
  (display (front-ptr queue)) (newline))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)

 ; -> shows (() b) because rear-ptr is not updated when we delete from the queue
(delete-queue! q)
(print-queue q)
