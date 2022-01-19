

(define (full-adder a b c-in sum c-out)
  (let [(s (make-wire)) (c1 (make-wire)) (c2 (make-wire))]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok)

; this doesn't compile bc the circuit thing isn't fully implemented
(define (ripple-carry-adder ak bk sk carry))
