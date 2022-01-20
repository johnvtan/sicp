#lang sicp

(#%require "wire.rkt")
(#%require "agenda.rkt")

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (validate-signal s)
  (if (or (= s 1) (= s 0))
    #t
    (error "Invalid signal" s)))

(define (logical-not s)
  (validate-signal s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]))

(define (inverter input output)
  (define (invert-input)
    (let [(new-value (logical-not (get-signal input)))]
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and s1 s2)
  (validate-signal s1)
  (validate-signal s2)
  (cond
    [(and (= s1 1) (= s2 1)) 1]
    [else 0]))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let [(new-value (logical-and (get-signal a1) (get-signal a2)))]
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (validate-signal s1)
  (validate-signal s2)
  (cond
    [(and (= s1 0) (= s2 0)) 0]
    [else 1]))

; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let [(new-value (logical-or (get-signal a1) (get-signal a2)))]
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let [(d (make-wire)) (e (make-wire))]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let [(s (make-wire)) (c1 (make-wire)) (c2 (make-wire))]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                (newline)
                (display name) (display " ")
                (display (current-time the-agenda))
                (display " New-value = ") (display (get-signal wire))
                (newline))))



(#%provide and-gate or-gate inverter half-adder probe full-adder)