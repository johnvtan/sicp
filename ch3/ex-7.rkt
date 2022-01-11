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
  
  (define (incorrect-password)
    (display "Incorrect password\n")
    #f)
  
  (define (dispatch pw-input m)
    (if (eq? pw-input password)
      (cond
        [(eq? m 'withdraw) withdraw]
        [(eq? m 'deposit) deposit]
        [(eq? m 'check-pw) #t]
        [else (error "Unknown request: MAKE-ACCOUNT" m)])
      (incorrect-password)))
    
  dispatch)

(define (make-joint acct old-pw new-pw)
  (define (dispatch pw-input m)
    (if (eq? pw-input new-pw)
      (acct old-pw m)
      "Incorrect password"))

  (if (acct old-pw 'check-pw)
    dispatch
    (error "Bad password for old acct")))

(define peter-acc (make-account 'open-sesame 100))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 15)
((peter-acc 'open-sesame 'withdraw) 5)
(make-joint peter-acc 'blep 'bloop)
