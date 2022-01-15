#!/usr/bin/racket

#lang sicp

(define (make-node item prev)
  (define (dispatch op)
    (cond
      [(eq? op 'item) item]
      [(eq? op 'prev) prev]
      [(eq? op 'set-prev!) (lambda (x) (set! prev x))]))
  dispatch)

(define (node-item n) (n 'item))
(define (node-prev n) (n 'prev))
(define (node-set-prev! n prev) ((n 'set-prev!) prev))

(define (make-deque)
  (let [(head '())
        (tail '())]
    
    (define (items)
      (map 
        node-item
        ;(lambda (n) 
        ;  (display (list 'N (node-item n))) (newline)
        ;  (list 'item (node-item n) 'prev (map node-item (node-prev n))))
        head))

    (define (empty?)
      (null? head))

    (define (front)
      (if (empty?)
        (error "FRONT called on empty deque" head)
        (node-item (car head))))
 
    (define (rear)
      (if (empty?)
        (error "REAR called on empty deque" head)
        (node-item (car tail))))
    
    (define (clear)
      (set! head '())
      (set! tail '()))
    
    (define (insert-rear item)
      (let [(new-pair (cons (make-node item tail) '()))]
        (cond
          [(empty?)
            (set! head new-pair)
            (set! tail new-pair)]
          [else
            (set-cdr! tail new-pair)
            (set! tail new-pair)]))
      (items))

    (define (insert-front item)
      (let [(new-node (cons (make-node item '()) head))]
        (cond
          [(empty?)
            (set! head new-node)
            (set! tail head)]
          [else
            (node-set-prev! (car head) new-node)
            (set! head new-node)]))
      (items))
    
    (define (delete-front)
      (cond 
        [(empty?) (error "DELETE-FRONT called on empty deque" (items))]
        [(eq? head tail) (clear)]
        [else
          (set! head (cdr head))
          (node-set-prev! (car head) '())])
      (items))
    
    (define (delete-rear)
      (cond
        [(empty?) (error "DELETE-REAR called on empty deque" (items))]
        [(eq? head tail) (clear)]
        [else
          (set! tail (node-prev (car tail)))
          (set-cdr! tail '())])
      (items))

    (define (dispatch op)
      (cond
        [(eq? op 'insert-rear) insert-rear]
        [(eq? op 'insert-front) insert-front]
        [(eq? op 'delete-front) delete-front]
        [(eq? op 'delete-rear) delete-rear]
        [(eq? op 'front) front]
        [(eq? op 'rear) rear]
        [(eq? op 'empty?) empty?]))
    dispatch))

(define (rear-deque d) ((d 'rear)))
(define (front-deque d) ((d 'front)))
(define (empty-deque? d) ((d 'empty?)))
(define (deque-insert-rear! d item) ((d 'insert-rear) item))
(define (deque-insert-front! d item) ((d 'insert-front) item))
(define (deque-delete-rear! d) ((d 'delete-rear)))
(define (deque-delete-front! d) ((d 'delete-front)))

(define d (make-deque))
(empty-deque? d)
(deque-insert-front! d 'a)
(deque-insert-front! d 'b)
(deque-insert-rear! d 'c)
(deque-insert-rear! d 'd)
(deque-delete-front! d)
(deque-delete-front! d)
(deque-insert-rear! d 'e)
(deque-delete-rear! d)
(deque-delete-rear! d)
(deque-delete-rear! d)
(deque-insert-rear! d 'g)
(deque-insert-front! d 'h)
(deque-delete-front! d)
(deque-delete-rear! d)