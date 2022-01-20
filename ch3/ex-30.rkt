#!/usr/bin/racket
#lang sicp

(#%require "circuits/wire.rkt")
(#%require "circuits/gates.rkt")
(#%require "circuits/agenda.rkt")
(#%require "../common-ops.rkt")

; this doesn't compile bc the circuit thing isn't fully implemented
(define (ripple-carry-adder ak bk sk cout)
  (if (or (not (= (length ak) (length bk) (length sk)))
               (= (length ak) 0))
    (error "boo bad input"))
  
  (define cin (make-wire))
  (define inner-cout (make-wire))
  
  (define (make-ripple-carry ak bk sk)
    (if (null? ak)
      'ripple-carry-done
      (let [(a (car ak))
            (b (car bk))
            (s (car sk))]

        (make-ripple-carry (cdr ak) (cdr bk) (cdr sk))
        (full-adder a b cin s inner-cout)
        (set! cin inner-cout)
        (set! inner-cout (make-wire)))))

  (make-ripple-carry (cdr ak) (cdr bk) (cdr sk))
  (full-adder (car ak) (car bk) cin (car sk) cout)

  'done)

(define a (list (make-wire) (make-wire) (make-wire)))
(define b (list (make-wire) (make-wire) (make-wire)))
(define s (list (make-wire) (make-wire) (make-wire)))
(define cin (make-wire))
(define cout (make-wire))

(ripple-carry-adder a b s cout)

(set-signal! (first a) 1)
(set-signal! (second a) 1)
(set-signal! (third a) 1)

(set-signal! (first b) 1)
(set-signal! (second b) 1)
(set-signal! (third b) 1)

(propagate)

(display (map get-signal a))
(display " + ")
(display (map get-signal b))
(display " = ")
(display (map get-signal (cons cout s)))
(newline)
