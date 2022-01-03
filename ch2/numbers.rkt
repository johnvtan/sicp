#!/usr/bin/racket

#lang scheme

(#%require (prefix internal- "complex-numbers-dd.rkt"))

; Global op-table + put/get
(define op-table (make-hash))

(define (put op type item)
  (hash-set! op-table (cons op type) item))

(define (get op type)
  (hash-ref op-table (cons op type) #f))

(define coercion-table (make-hash))
(define (put-coercion from to func)
  (hash-set! coercion-table (cons from to) func))
(define (get-coercion from to)
  (hash-ref! coercion-table (cons from to) #f))

; tags
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    [(pair? datum) (car datum)]
    [(number? datum) 'scheme-number]
    [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (apply-generic op . args)
  (let [(type-tags (map type-tag args))]
    (let [(proc (get op type-tags))]
      (if proc
        (apply proc (map contents args))
        ; look for conversions between the types
        ; This only does a single depth search for conversion
        ; If you really wanted to go nuts with this, you could frame
        ; this as a graph search and try to find a path from t1 to t2
        ; in the case that there's no direct conversion from t1->t2 or t2->t1,
        ; but there exists some intermediate type t3 for which t1->t3/t3->t1 and
        ; t2->t3/t3->t2 exists.
        (if (= (length args) 2)
          (let [(type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args))]
            (let [(t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1))]
              (cond
                [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                [else (error "No method for these types" (list op type-tags))])))
          (error "No method for these type tags" (list op type-tags)))))))
      
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) x))

  'scheme-number)
(install-scheme-number-package)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let [(g (gcd n d))]
      (cons (/ n g) (/ d g))))
  
  (put-coercion 'scheme-number 'rational
    (lambda (x) (make-rational x 1)))

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

  (put-coercion 'scheme-number 'complex
    (lambda (x) (make-complex-from-real-imag x 0)))
  
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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
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

coercion-table

(#%provide add sub mul div make-scheme-number make-rational
           make-complex-from-real-imag make-complex-from-mag-ang
           equ? magnitude angle real-part imag-part =zero?)