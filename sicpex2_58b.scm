(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
          (else
           (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (not (null? (addend x))))

(define (addend s)
  (define (addend-internal addend-so-far s)
    (cond ((not (pair? s)) '())
          ((eq? (car s) '+)
           (if (and (pair? addend-so-far) (null? (cdr addend-so-far)))
               (car addend-so-far)
               addend-so-far))
          (else 
           (addend-internal (append addend-so-far (list (car s))) (cdr s)))))
  (addend-internal '() s))

(define (augend s)
  (if (eq? (car s) '+)
      (if (null? (cddr s))
          (cadr s)
          (cdr s))
      (augend (cdr s))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) 
       (pair? (cdr x))
       (pair? (cddr x))
       (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

; There's a better solution at http://community.schemewiki.org/?sicp-ex-2.58 which 
; handles the operator precedence properly (the above relies on the order of clauses 
; in the deriv function which is fragile)