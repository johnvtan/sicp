#!/usr/bin/racket

#lang sicp

(define (make-table)
  (let [(local-table (list '*table*))]
    (define (assoc key)
      (define (iter records)
        (cond
          [(null? records) #f]
          [(equal? key (caar records)) (car records)]
          [else (iter (cdr records))]))
      (iter (cdr local-table)))

    (define (lookup key)
      (let [(record (assoc key))]
        (if record
          (cdr record)
          #f)))
   
    (define (insert! key value)
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

(define (table-get table keys)
  (define (iter value keys)
    (if (null? keys)
      value
      ; would be nice if I could check if new-value is a table, but idk how to do that
      (let [(new-value ((value 'lookup) (car keys)))]
        (iter new-value (cdr keys)))))
  
  (if (null? keys)
    (error "Called table-get with no keys")
    (iter table keys)))

(define (table-put table keys value)
  (define (iter table keys)
    (if (null? (cdr keys))
      ((table 'insert!) (car keys) value)
      (let [(new-table ((table 'lookup) (car keys)))]
        (iter new-table (cdr keys)))))
  (if (null? keys)
    (error "Called table-put with no keys")
    (iter table keys)))

(define (table-items table) ((table 'items)))


(define t1 (make-table))
(table-put t1 '(a) 3)
(table-put t1 '(b) 4)
(table-items t1)

(define t2 (make-table))
(table-put t2 (list 123) 'baz)
(table-put t2 (list 456) 'foo)
(table-items t2)

(define t3 (make-table))
(table-put t3 '(letters) t1)
(table-put t3 '(numbers) t2)
(table-get t3 '(letters a))
(table-get t3 '(numbers 456))
(table-put t3 '(letters c) 112345)
(table-get t3 '(letters c))
(table-get t1 '(c))

(define t4 (make-table))
(table-put t4 '(other-table) t3)
(table-put t4 '(big) 'bad)
(table-get t4 '(big))
(table-get t4 '(other-table numbers 123))
(table-put t4 '(other-table numbers 123) 'bazbaz)
(table-get t4 '(other-table numbers 123))
(table-get t2 '(123))