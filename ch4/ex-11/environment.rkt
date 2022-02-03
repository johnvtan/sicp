#!/usr/bin/racket

#lang sicp 

; An environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; A frame is a pair of lists -- variables and associated values
(define (make-frame variables values)
  (map list variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cadr frame))
(define (frame-pair-var frame-pair) (car frame-pair))
(define (frame-pair-val frame-pair) (cadr frame-pair))

; Assumes frame not empty
;(define (add-binding-to-frame! var val frame)
;  (display (list 'add-binding var val frame)) (newline)
;  (set-cdr! frame (cons (list var val) (cdr frame))))

(define (add-binding-to-first-frame! env var val)
  (set-car! env (cons (list var val) (first-frame env))))

; Extending an environment means adding a new frame
; Typically occurs during procedure application
(define (extend-environment vars vals base-env)
  ;(display (list 'extend-environment (cons (make-frame vars vals) base-env))) (newline)
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
    (define (scan frame)
      (cond
        [(null? frame) (env-loop (enclosing-environment env))]
        [(eq? var (frame-pair-var (car frame))) (frame-pair-val (car frame))]
        [else (scan (cdr frame))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let [(frame (first-frame env))]
        (scan frame))))
  (env-loop env))

; Setting a variable is almost the same, except that we change
; the corresponding value when we find it
(define (set-variable-value! var val env)
  (define (env-loop env)  
    (define (scan frame)
      (cond
        [(null? frame) (env-loop (enclosing-environment env))]
        [(eq? var (frame-pair-var (car frame))) (set-car! (cdr (car frame)) val)]
        [else (scan (cdr frame))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let [(frame (first-frame env))]
        (scan frame))))
  (env-loop env))

; Defines a variable in the first frame of an environment
; If the variable already exists in the frame, the value is changed
; This does not look in enclosing frames and will add a binding in
; the first frame if one does not already exist.
(define (define-variable! var val env)
  ;(display (list 'define-variable var val)) (newline)
  (let [(frame (first-frame env))]
    (define (scan curr)
      ;(display (list 'scan curr)) (newline)
      (cond
        [(null? curr) (add-binding-to-first-frame! env var val)]
        [(eq? var (frame-pair-var (car curr))) (set-car! (cdr (car curr)) val)]
        [else (scan (cdr curr))]))
    (scan frame)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'eq? eq?)))

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
    ;(display (list 'setup-environment initial-env)) (newline)
    initial-env))

(define the-global-environment (setup-environment))
  
(#%provide define-variable! set-variable-value! extend-environment lookup-variable-value
          the-global-environment setup-environment)