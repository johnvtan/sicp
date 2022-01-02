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
            (iter next-type (compose raise-func acc)))
          #f))))
  (iter from (lambda (x) x)))

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
    [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (apply-generic op . args)
  ; I'll implement a different strategy for handling multiple arguments
  ; of different types
  ; Instead of trying to convert every argument to a single type, I'll just
  ; go left to right and convert as necessary

  (define (apply-bin-op-same-type type a1 a2)
    ;(display (list 'apply-bin-op-same-type (list type a1 a2)))
    (let [(proc (get op (list type type)))]
      (if proc
        (proc a1 a2)
        (error "No method for these types" (list op type type)))))

  (define (convert-and-apply-bin-op type1 a1 type2 a2)
    (let [(t1->t2 (get-raise-func type1 type2))
          (t2->t1 (get-raise-func type2 type1))]
      ;(display (list 'convert-and-apply-bin-op t1->t2 t2->t1 type1 type2)) (newline)
      (cond
        [t1->t2 (apply-bin-op-same-type type2 (contents (t1->t2 a1)) a2)]
        [t2->t1 (apply-bin-op-same-type type1 a1 (contents (t2->t1 a2)))]
        [else (error "No conversion for these types" (list op type1 type2))])))

  (define (apply-op-multiple-args args)
    (foldl
      (lambda (arg acc)
        (let [(type1 (type-tag acc))
              (type2 (type-tag arg))
              (a1 (contents acc))
              (a2 (contents arg))]
        ;(display (list 'apply-op-multiple-args op arg acc)) (newline)
        (if (eq? type1 type2)
          (apply-bin-op-same-type type1 a1 a2)
          (convert-and-apply-bin-op type1 a1 type2 a2))))
      (car args)
      (cdr args)))
  
  (define (apply-op-single-arg arg)
    (let [(proc (get op (list (type-tag arg))))]
      (if proc
        (proc (contents arg))
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
  (put 'equ? '(real real) =)
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'make 'real (lambda (x) x))

  (add-raise-func 'integer 'rational (lambda (x) (make-rational x 1)))
  (add-raise-func 'real 'complex (lambda (x) (make-complex-from-real-imag x 0)))
  'scheme-number)

(install-scheme-number-package)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let [(g (gcd n d))]
      (cons (/ n g) (/ d g))))
  
  ;(put-coercion 'scheme-number 'rational
  ;  (lambda (x) (make-rational x 1)))

  ; feels weird to be mixing my number system with builtins, but...
  (add-raise-func 'rational 'real (lambda (r) (/ (numer r) (denom r))))

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
  'rational)
(install-rational-package)

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
(install-complex-package)

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

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; The only thing you should need to do if you want to add new levels to the tower
; is to add a conversion functoin using add-raise-func
(add 3 4 (make-rational 3 4) 3.21 (make-complex-from-real-imag 3 1))
(sub 10 4 (make-rational 3 4) 3.21 (make-complex-from-real-imag 3 1))
(mul 3 4 (make-rational 3 4) 3.21 (make-complex-from-real-imag 3 1))
(equ? 3 (make-rational 9 3))
(equ? (make-complex-from-real-imag 3 0) (make-rational 12 4))
(equ? (make-complex-from-real-imag 3 1) (make-rational 12 4))
(equ? 3 (make-rational 9 4))
(div 4 5)
(div 8 2)
(div 1.23 3.45)
 
; I'm not really sure how this should work?
; scheme returns a rational numer (4/5) from (/ 4 5)
; which is apparently != 0.8
; I could force everything to be inexact (using exact->inexact)
; but I'm not sure that's right?
; (equ? (make-rational 4 5) 0.8)
; (= (/ 4 5) 0.8)