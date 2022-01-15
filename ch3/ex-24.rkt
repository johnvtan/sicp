#!/usr/bin/racket

#lang sicp

(define (make-table same-key?)
  (let [(local-table (list '*table*))]
    (define (assoc key)
      (define (iter records)
        (cond
          [(null? records) #f]
          [(same-key? key (caar records)) (car records)]
          [else (iter (cdr records))]))
      (iter (cdr local-table)))

    (define (lookup key)
      (let [(record (assoc key))]
        (if record
          (cdr record)
          #f)))
   
    (define (insert! key value)
      (display (list 'insert! key value)) (newline)
      (let [(record (assoc key))]
        (if record
          (set-cdr! record value)
          (set-cdr! local-table (cons
                                  (cons key value)
                                  (cdr local-table))))))
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

(define t1 (make-table equal?))
(table-put t1 'a 1)
(table-put t1 'b 2)
(table-items t1)
(table-put t1 'a 3)
(table-get t1 'a)
(table-get t1 'b)

(define t2 (make-table (lambda (k1 k2) (< (abs (- k1 k2)) 10))))
(table-put t2 10 'a)
(table-put t2 25 'b)
(table-put t2 15 'c)
(table-put t2 30 'd)
(table-get t2 27)
(table-items t2)
