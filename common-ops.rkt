; Common operations that are used in SICP, but not part of Racket's SICP dialect
#lang sicp

(#%provide accumulate accumulate-n filter fold-left fold-right flatmap enumerate-interval
  first second third fourth)

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

 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 

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

(define (first lst)
  (car lst))

(define (second lst)
  (cadr lst))

(define (third lst)
  (caddr lst))

(define (fourth lst)
  (cadddr lst))
