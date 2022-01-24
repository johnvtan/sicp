#!/usr/bin/racket

#lang sicp

; CONNECTORS
(define (for-each-except exception proc lst)
  (define (loop items)
    (cond
      [(null? items) 'done]
      [(eq? (car items) exception) (loop (cdr items))]
      [else (proc (car items))
            (loop (cdr items))]))
  (loop lst))

(define (make-connector)
  (let [(value false) (informant false) (constraints '())]
    (define (set-my-value newval setter)
      (cond
        [(not (has-value? me))
          (set! value newval)
          (set! informant setter)
          (for-each-except setter
                           inform-about-value
                           constraints)]
        [(not (= value newval))
          (error "Contradiction" (list value newval))]
        [else 'ignored]))
    
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant false)
          (for-each-except retractor
                           inform-about-no-value
                           constraints))

        ; only propagates forgotten value if retractor was informant
        'ignored))
    
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      
      ; if there's a new constraint that's added, inform that constraint
      ; about my value when it is added
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    
    (define (me request)
      (cond
        [(eq? request 'has-value?) (if informant true false)]
        [(eq? request 'value) value]
        [(eq? request 'set-value!) set-my-value]
        [(eq? request 'forget) forget-my-value]
        [(eq? request 'connect) connect]
        [else (error "Unknown operation: CONNECTOR" request)]))
    
    me))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector newval setter) ((connector 'set-value!) newval setter))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


; CONSTRAINT IMPLEMENTATIONS
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      [(and (has-value? a1) (has-value? a2))
        (set-value! sum
                    (+ (get-value a1) (get-value a2))
                    me)]
      [(and (has-value? a1) (has-value? sum))
        (set-value! a2
                    (- (get-value sum) (get-value a1))
                    me)]
      [(and (has-value? a2) (has-value? sum))
        (set-value! a1
                    (- (get-value sum) (get-value a2))
                    me)]))
  
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request ADDER" request)]))
  
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      [(or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
        (set-value! product 0 me)]
      [(and (has-value? m1) (has-value? m2))
        (set-value! product
                    (* (get-value m1) (get-value m2))
                    me)]
      [(and (has-value? product) (has-value? m1))
        (set-value! m2
                    (/ (get-value product) (get-value m1))
                    me)]
      [(and (has-value? product) (has-value? m2))
        (set-value! m1
                    (/ (get-value product) (get-value m2))
                    me)]))
  
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request: MULTIPLIER" request)]))
  
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value)
    (newline))
  
  (define (process-new-value)
    (print-probe (get-value connector)))
  
  (define (process-forget-value)
    (print-probe "?"))
  
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request: PROBE" request)]))
  
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (averager a b c)
  (let [(one-half (make-connector))
        (a+b (make-connector))]
    (adder a b a+b)
    (constant (/ 1 2) one-half)
    (multiplier a+b one-half c)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe 'a-value a)
(probe 'b-value b)
(probe 'c-value c)

(averager a b c)

(set-value! a 100 'user)
(set-value! b 10 'user)

(forget-value! b 'user)
(set-value! c 88 'user2)