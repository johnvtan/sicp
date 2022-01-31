; Louis plans to redefine eval as such:
(define (eval exp env)
  (cond

    ; Primitives?
    [(self-evaluating? exp) exp]

    ; Lookup the variable value in the environment
    ; Are primitive operators (+, -, etc) variables?
    [(variable? exp) (lookup-variable-value exp env)]

    ; Quotes get the text of the symbol, e.g. 'text -> text
    [(quoted? exp) (text-of-quotation exp)]

    [(definition? exp) (eval-definition exp env)]
    [(if? exp) (eval-if exp env)]

    ; Creates a procedure, which can be passed to apply
    [(lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env)]
    [(begin? exp) (eval-sequence (begin-actions exp) env)]

    ; Converts cond statements into ifs
    [(cond? exp) (eval (cond->if exp) env)]

    ; Note how the operator is also evaluated
    ; This is how ((if x + -) 3 2) would be evaluated
    [(application? exp)
      (apply (eval (operator exp) env)
             (list-of-values (operands exp) env))]

    [(assignment? exp) (eval-assignment exp env)] ; <-- Louis's change
    [else (error "Unknown expression type EVAL" exp)]))
  
; But this won't work because application is a more general expression than
; assignment. An assignment special form would be interpreted as an application
; of a procedure set! or define, which would cause an error when looking up that
; procedure in the environment.
; It seems like special forms have to all come before the application clause in eval.

; If we want this to work, we can redefine application?, operator, and operands
; and change the syntax of a procedure application to precede with 'call.
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
