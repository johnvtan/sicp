#!/usr/bin/racket

#lang scheme

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
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (let [(g (gcd n d))]
      (list (/ n g) (/ d g))))

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
  (define (square x) (* x x))
  (define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cadr z))
    (define (make-from-real-imag x y) (list x y))
    (define (magnitude z)
      (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (define (angle z)
      (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))
    
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
      (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
      (lambda (r a) (tag (make-from-mag-ang r a))))
    'rectangular)

  (define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cadr z))
    (define (make-from-mag-ang r a) (list r a))

    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) 
      (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
      (cons (sqrt (+ square x) (square y))
            (atan y x)))
    
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
      (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
      (lambda (r a) (tag (make-from-mag-ang r a))))
    'polar)

  (install-rectangular-package)
  (install-polar-package)

  (define (tag z) (attach-tag 'complex z))
  ; This two level apply-generic application is a little confusing,
  ; but because there are two reprs for complex numbers, we need to
  ; dispatch again based on the type of the complex number.
  ; So if we call a function (real-part z), where z is some complex
  ; number of the form e.g. (complex rectangular (a . b))
  ;
  ; Then the function in op-table that maps to ('real-part . '(complex))
  ; will be given an argument that has the the contents of z with the
  ; type tag stripped off, e.g. (rectangular (a . b))
  ; 
  ; So then to call the correct real-part function installed by
  ; the install-rectangular-package function, we need to call
  ; apply-generic **again** so that we lookup the real-part 
  ; in the op-table that takes a "rectangular", and pass to it
  ; the contents of the rectangular number, e.g. (a . b)
  (define (apply-generic-again op)
    (lambda (x) (apply-generic op x)))
  
  (put 'real-part '(complex) (apply-generic-again 'real-part))
  (put 'imag-part '(complex) (apply-generic-again 'imag-part))
  (put 'magnitude '(complex) (apply-generic-again 'magnitude))
  (put 'angle '(complex) (apply-generic-again 'angle))
  
  (put 'equ? '(complex complex)
    (lambda (z1 z2) (and (eq? (real-part z1) (real-part z2))
                         (eq? (imag-part z1) (imag-part z2)))))

  (define (add-complex z1 z2)
    (make-complex-from-real-imag (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-complex-from-real-imag (- (real-part z1) (real-part z2))
                        (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2))
                      (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2))
                      (- (angle z1) (angle z2))))
  
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)
  (put 'div '(complex complex) div-complex)
    
  (put '=zero? '(complex)
    (lambda (z) (= (magnitude z) 0)))
  
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag ((get 'make-from-real-imag 'rectangular) x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag ((get 'make-from-mag-ang 'polar) r a))))

  ; I hope this figures out whatever the correct thing to do is lmao
  (add-project-func 'complex 'real real-part)
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

(define z1 (make-complex-from-real-imag 1 0))
(add z1 z1)
(mul z1 z1)
(add z1 (make-complex-from-real-imag 1 -1))
(add 3 z1)
(add 3 z1 (make-rational 4 5) 0.23)
; (magnitude z)
; (angle z)
; (imag-part z)
