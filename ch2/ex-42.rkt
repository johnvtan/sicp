#!/usr/bin/racket

#lang sicp

(#%require "../common-ops.rkt")

(define (queens board-size)

  ; Determines if positions is safe
  ; This makes two assumptions about 'positions':
  ;   1. The first element in positions is new and has not been checked to be safe yet
  ;   2. The rest of the elements in positions have already been checked to be safe
  (define (safe? positions)
    (define (iter new-pos rest-of-pos)
      (if (null? rest-of-pos)
        #t
        (let [(pos (car rest-of-pos))]
          (cond
            ; Check on horizontal/vertical if the new queen is on same line
            ; as the old queen
            [(or (= (first new-pos) (first pos))
                 (= (second new-pos) (second pos))) #f]
            ; Check the diagonals
            [(= (abs (- (first new-pos) (first pos)))
                (abs (- (second new-pos) (second pos)))) #f]
            [else (iter new-pos (cdr rest-of-pos))]))))

    (iter (car positions) (cdr positions)))

  ; Appends (row, col) to front of rest-of-queens
  (define (adjoin-position row col rest-of-queens)
      (cons (list row col) rest-of-queens))

  (define (queen-cols k)
    (if (= k 0)
      (list '()) 
      (filter
        (lambda (positions) (safe? positions))
        (flatmap
          (lambda (rest-of-queens)
            ; For each row, in column k, add a queen in that row
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          ; Recursively generate queens for previous cols
          ; queen-cols should only be returning positions which have been checked
          ; to be safe already
          (queen-cols (- k 1))))))

  (queen-cols board-size))

(length (queens 1))
(length (queens 2))
(length (queens 3))
(length (queens 4))
(length (queens 8))
(length (queens 11))