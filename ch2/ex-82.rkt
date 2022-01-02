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
    (let [(t1->t2 (get-coercion type1 type2))
          (t2->t1 (get-coercion type2 type1))]
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


; (#%provide add sub mul div make-scheme-number make-rational
;            make-complex-from-real-imag make-complex-from-mag-ang
;            equ? magnitude angle real-part imag-part =zero?)

(div 3 4 5 6)
(add 3 (make-rational 4 6) 7 (make-rational 8 10))
(add (make-complex-from-real-imag 2 3) 3 10 (make-complex-from-real-imag 4 5))

; The strategy explained in the book (trying to convert all to a single arg type at first)
; would not work if there's some type a->b->c, and c->b->a, but only args of
; type a and c are used (where -> means convertible to)
; The implemented solution here has the same problem, and additionally has the problem that
; we don't get up-front information about whether the whole computation can be performed.
; If we get an error with the implemented solution (i.e, converting two at a time), it will
; only be after potentially doing a lot of the computation, whereas with the suggestede solution
; we will know whether the computation is doable before doing the actual procedure. 

; The advantage of the solution implemented here is that only binary operators need to be defined
; There's no need to register
