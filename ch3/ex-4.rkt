#!/usr/bin/racket

#lang scheme

(define (make-account password balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)))

  (define (call-the-cops)
    "wee woo wee woo")
  
  (let ([incorrect-calls 0])
    (define (incorrect-password op)
      (set! incorrect-calls (+ incorrect-calls 1))
      (if (>= incorrect-calls 7)
        (call-the-cops)
        "Incorrect password"))

    (define (dispatch pw-input m)
        (if (eq? pw-input password)
          (begin
            (set! incorrect-calls 0)
            (cond
              [(eq? m 'withdraw) withdraw]
              [(eq? m 'deposit) deposit]
              [else (error "Unknown request: MAKE-ACCOUNT" m)]))
          incorrect-password))
      dispatch))
 
(define acc (make-account 'secret 100))
((acc 'secret 'withdraw) 40)
((acc 'secret 'withdraw) 20)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)
((acc 'secret 'deposit) 50)
((acc 'not-secret 'deposit) 50)



