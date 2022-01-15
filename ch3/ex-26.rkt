#!/usr/bin/racket

#lang sicp

(define (make-table same-key? lt?)
  (let [(local-table (list '*table*))]

    (define (make-record key value left right) (list key value left right))
    (define (record-key rec) (car rec))
    (define (record-value rec) (cadr rec))
    (define (record-left rec) (caddr rec))
    (define (record-right rec) (cadddr rec))

    (define (set-record-value! rec val) (set-car! (cdr rec) val))
    (define (set-record-left! rec left) (set-car! (cddr rec) left))
    (define (set-record-right! rec right) (set-car! (cdddr rec) right))

    (define (assoc key)
      (define (iter parent curr-record)
        (if (null? curr-record)
          (cons parent #f) 
          (cond
            [(same-key? key (record-key curr-record)) (cons parent curr-record)]
            [(lt? key (record-key curr-record)) (iter curr-record (record-left curr-record))]
            [else (iter curr-record (record-right curr-record))])))
      (iter '() (cdr local-table)))
    
    (define (lookup key)
      (let [(ret (assoc key))]
        (let [(record (cdr ret))]
          (if record
            (record-value record)
            #f))))
   
    (define (insert! key value)
      (let [(ret (assoc key))]
        (let [(parent (car ret))
              (record (cdr ret))]
          (if record
            (set-record-value! record value)
            (cond
              [(null? parent) (set-cdr! local-table (make-record key value '() '()))]
              [(lt? key (record-key parent)) (set-record-left! parent (make-record key value '() '()))]
              [else (set-record-right! parent (make-record key value '() '()))])))))

    (define (items)
      (cdr local-table))
    
    (define (dispatch m)
      (cond
        [(eq? m 'lookup) lookup]
        [(eq? m 'insert!) insert!]
        [(eq? m 'items) items]
        [else (error "Unknown table op" m)]))

    dispatch))

(define (table-get table key) ((table 'lookup) key))
(define (table-put table key value) ((table 'insert!) key value))
(define (table-items table) ((table 'items)))

(define t1 (make-table eq? <))
(table-put t1 10 'a)
(table-put t1 15 'b)
(table-put t1 5 'c)
(table-put t1 30 'd)
(table-items t1)
(table-get t1 10)
(table-get t1 30)
(table-put t1 15 'NEW)
(table-get t1 15)
(table-items t1)
