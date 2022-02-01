#!/usr/bin/racket

#lang sicp 

; An environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; A frame is a pair of lists -- variables and associated values
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; Extending an environment means adding a new frame
; Typically occurs during procedure application
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

; Looking up a var in an environment involves looking at the variables
; in each frame in the environment successively until we reach the
; outermost environment.
(define (lookup-variable-value var env)
  (define (env-loop env)  
    (define (scan vars vals)
      (cond
        [(null? vars) (env-loop (enclosing-environment env))]
        [(eq? var (car vars)) (car vals)]
        [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let [(frame (first-frame env))]
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; Setting a variable is almost the same, except that we change
; the corresponding value when we find it
(define (set-variable-value! var val env)
  (define (env-loop env)  
    (define (scan vars vals)
      (cond
        [(null? vars) (env-loop (enclosing-environment env))]
        [(eq? var (car vars)) (set-car! vals val)]
        [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let [(frame (first-frame env))]
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; Defines a variable in the first frame of an environment
; If the variable already exists in the frame, the value is changed
; This does not look in enclosing frames and will add a binding in
; the first frame if one does not already exist.
(define (define-variable! var val env)
  (let [(frame (first-frame env))]
    (define (scan vars vals)
      (cond
        [(null? vars) (add-binding-to-frame! var val frame)]
        [(eq? var (car vars)) (set-car! vars val)]
        [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame) (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'eq? eq?)
        (list 'assoc assoc)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (setup-environment)
  (let [(initial-env
          (extend-environment
            (primitive-procedure-names) 
            (primitive-procedure-objects)
            the-empty-environment))]
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))
  
(#%provide define-variable! set-variable-value! extend-environment lookup-variable-value
          the-global-environment setup-environment)