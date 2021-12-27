; Common operations that are used in SICP, but not part of Racket's SICP dialect
#lang sicp

(#%provide accumulate accumulate-n fold-left fold-right flatmap enumerate-interval)

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (accumulate op init (map car seqs)))
          (accumulate-n op init (accumulate op init (map cdr seqs))))))

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      ; Why would the book switch the order of arguments passed to op?
      ; I don't think this is a good decision -- it causes confusion for no gain.
      ; Is it really clearer that, for operations passed to fold-left, the accumulator
      ; is the left-most parameter? I don't think it makes sense. It would be simpler
      ; if ops were interchangeable between fold-left/right and that the argument order
      ; is consistent.
      ; NOTE I changed this for common-ops
      (iter (op (car rest) result) (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Generates list of interval [start, end]
(define (enumerate-interval start end)
  (define (iter acc k)
    (if (< k start)
      acc
      (iter (cons k acc) (- k 1))))
  (iter '() end))