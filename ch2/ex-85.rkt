#!/usr/bin/racket

#lang scheme

(#%require (prefix internal- "complex-numbers-dd.rkt"))

; Global op-table + put/get
(define op-table (make-hash))

(define (put op type item)
  (hash-set! op-table (cons op type) item))

(define (get op type)
  (hash-ref op-table (cons op type) #f))

; TOWER
(define tower-conversions (make-hash))

; Structure of tower-conversions:
; key = (from-type . 'raise)
; value = (to-type . function)
(define (add-raise-func from to func)
  (hash-set! tower-conversions (cons from 'raise) (cons to func)))

(define (add-project-func from to func)
  (hash-set! tower-conversions (cons from 'project) (cons to func)))

; Returns a composed function that converts from->to if a
; chain of conversion functions exists
; Otherwise, this returns false.
; Returning false implies that there is no way to raise from->to
; and that therefore "from" is higher than "to" in the hierarchy
(define (get-raise-func from to)
  ;(display (list '****get-raise-func from to)) (newline)
  (define (iter curr-type acc)
    ;(display (list 'get-raise-func-iter curr-type to)) (newline)
    (if (eq? curr-type to)
      acc
      (let [(from-tower (hash-ref tower-conversions (cons curr-type 'raise) #f))]
        (if from-tower
          (let [(next-type (car from-tower))
                (raise-func (cdr from-tower))]
            (iter next-type (lambda (x) 
              (raise-func (contents (acc x))))))
          #f))))
  (iter from (lambda (x)  x)))

(define (get-project-type-and-func from)
  (hash-ref! tower-conversions (cons from 'project) #f))

; Simplifies the value with the given type as far as possible
; If it can't be simplified anymore, then it returns the original value
; and type
(define (drop tagged-value)
  (let [(project (get-project-type-and-func (type-tag tagged-value)))]
    (if project
      (let [(project-func (cdr project))
            (project-type (car project))]
        
        ; Find the raise function to convert from the projected type to the
        ; type of the tagged value
        (let [(raise-func (get-raise-func project-type (type-tag tagged-value)))
              (projected (project-func (contents tagged-value)))]
          ; (display 'DROP-inner) (newline)
          ; (display (list tagged-value (raise-func projected))) (newline)
          ; (display (equ? tagged-value (raise-func projected))) (newline)
          ; (display 'DROPREADY) (newline)
          (cond
            [(not raise-func) (error "No raise func for projected? bad")]

            ; If we are able to project then raise and retrieve the same value,
            ; then try to drop the projected value again
            [(equ? tagged-value (raise-func projected)) (drop projected)]

            ; Otherwise, projecting then raising does not yield the same value
            ; so we can't simplify the value anymore
            [else tagged-value])))

      ; No existing project function, so we cannot simplify the tagged value
      tagged-value)))

; tags
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    [(pair? datum) (car datum)]
    [(integer? datum) 'integer]
    [(number? datum) 'real]
    [else #f]))

(define (contents datum)
  (cond
    [(not (type-tag datum)) (error "Bad tagged datum: CONTENTS" datum)]
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Also bad datum? CONTENTS: " datum)]))

(define (apply-generic op . args)
  ; I'll implement a different strategy for handling multiple arguments
  ; of different types
  ; Instead of trying to convert every argument to a single type, I'll just
  ; go left to right and convert as necessary
  ;(display (list 'apply-generic op args)) (newline)
  (define (apply-bin-op-same-type type a1 a2)
    ;(display (list 'apply-bin-op-same-type (list type a1 a2)))
    (let [(proc (get op (list type type)))]
      (if proc
        (let [(result (proc a1 a2))]

          ; Only try to drop if it's typed 
          ; if we're returning a bool or something else from this system, don't try to drop
          (if (type-tag result)
            (drop result)
            result))
        (error "No method for these types" (list op type type)))))

  (define (convert-and-apply-bin-op a1 a2)
    (let [(type1 (type-tag a1))
          (type2 (type-tag a2))]
      (let [(t1->t2 (get-raise-func type1 type2))
            (t2->t1 (get-raise-func type2 type1))]
        ;(display (list 'convert-and-apply-bin-op t1->t2 t2->t1 a1 a2)) (newline)
        ;(display (t2->t1 a2)) (newline)
        (cond
          [t1->t2 (apply-bin-op-same-type type2 (contents (t1->t2 a1)) (contents a2))]
          [t2->t1 (apply-bin-op-same-type type1 (contents a1) (contents (t2->t1 a2)))]
          [else (error "No conversion for these types" (list op type1 type2))]))))

  (define (apply-op-multiple-args args)
    (foldl
      (lambda (arg acc)
        (let [(type1 (type-tag acc))
              (type2 (type-tag arg))]
        ;(display (list 'apply-op-multiple-args op arg acc)) (newline)
        (if (eq? type1 type2)
          (apply-bin-op-same-type type1 (contents acc) (contents arg))
          (convert-and-apply-bin-op acc arg))))
      (car args)
      (cdr args)))

  (define (apply-op-single-arg arg)
    ;(display (list `apply-op-single-arg op arg)) (newline)
    (let [(proc (get op (list (type-tag arg))))]
      (if proc
        (let [(result (proc (contents arg)))]
          (if (type-tag result)
            (drop result)
            result))
        (error "No method found for this type" (list op (type-tag arg))))))
  
  ;(display (list 'apply-generic op args)) (newline)
  (cond
    [(null? args) (error "No 0-argument methods implemented" (list op))]
    [(null? (cdr args)) (apply-op-single-arg (car args))]
    [else (apply-op-multiple-args args)]))
     
(define (install-scheme-number-package)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  ; should this naturally convert to a rational number?
  ; like scheme's implementation??
  (put 'div '(integer integer)
    (lambda (x y)
      (cond
        ; I wonder how bad it is to compute the remainder here
        ; should I just do the division and throw it away if
        ; we need to make a rational?
        [(and (> x y) (= (remainder x y) 0)) (/ x y)]
        [else (make-rational x y)])))

  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'make 'integer (lambda (x) x))

  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)

  ; I guess reals are floats, so / will also return a float
  (put 'div '(real real) /)

  ; TODO inexact equality for reals with arbitrary threshold
  (put 'equ? '(real real) 
    (lambda (a b) (< (abs (- a b)) 0.0001)))

  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'make 'real (lambda (x) x))

  (add-raise-func 'integer 'rational (lambda (x) (make-rational x 1)))
  (add-raise-func 'real 'complex (lambda (x) (make-complex-from-real-imag x 0)))
  'scheme-number)


(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let [(g (gcd n d))]
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ; assuming they are all reduced
  (define (equ-rat? x y)
    (and (eq? (numer x) (numer y))
         (eq? (denom x) (denom y))))
  
  (define (=zero? x)
    (= (numer x) 0))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero?)

  ; feels weird to be mixing my number system with builtins, but...
  (add-raise-func 'rational 'real (lambda (r) (/ (numer r) (denom r))))

  (add-project-func 'rational 'integer
    (lambda (x) (round (/ (numer x) (denom x)))))

  (add-project-func 'real 'rational
    (lambda (x) (let [(rat (rationalize x 1/100))]
      (make-rational (inexact->exact (numerator rat)) (inexact->exact (denominator x))))))

  'rational)

(define (install-complex-package)
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (internal-add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (internal-sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (internal-mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (internal-div-complex z1 z2))))

  ; (put-coercion 'scheme-number 'complex
  ;   (lambda (x) (make-complex-from-real-imag x 0)))

  (add-project-func 'complex 'real internal-real-part)
  
  (put 'real-part '(complex) internal-real-part)
  (put 'imag-part '(complex) internal-imag-part)
  (put 'magnitude '(complex) internal-magnitude)
  (put 'angle '(complex) internal-angle)
  
  (put 'equ? '(complex complex)
    (lambda (z1 z2) (and (eq? (internal-real-part z1) (internal-real-part z2))
                         (eq? (internal-imag-part z1) (internal-imag-part z2)))))
  
  (put '=zero? '(complex)
    (lambda (z) (= (internal-magnitude z) 0)))
  
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (internal-make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (internal-make-from-mag-ang r a))))
  'complex)

(define (add . args) (apply apply-generic 'add args))
(define (sub . args) (apply apply-generic 'sub args))
(define (mul . args) (apply apply-generic 'mul args))
(define (div . args) (apply apply-generic 'div args))

; This is a little weird? interface is how 2 argument requirement
; is enforced, but in apply-generic equ? could take any number of args
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

; (define (make-scheme-number x)
;   (drop ((get 'make 'scheme-number) x)))

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(display "READY\n\n")

(drop 3)
(drop (make-rational 3 1))
(drop (make-rational 3 2))
(drop (make-complex-from-real-imag 3 0))
(drop (make-complex-from-real-imag 3 1))
(drop 3.5)


(add (make-rational 3 1) (make-complex-from-real-imag 3 0))
(sub (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 15 3))
(add (make-rational 1 2) (make-rational 1 2))

; TODO this is having problems due to finding equality to inexact numbers
; and conversions to/from polar, I think
(mul (make-complex-from-real-imag 0 1) (make-complex-from-real-imag 0 1))
