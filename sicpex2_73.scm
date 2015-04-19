(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) false))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
 
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s-operands) (car s-operands))

(define (augend s-operands) 
  (if (null? (cddr s-operands))
      (cadr s-operands)
      (cons '+ (cdr s-operands))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p-operands) (car p-operands))

(define (multiplicand p-operands) 
  (if (null? (cddr p-operands))
      (cadr p-operands)
      (cons '* (cdr p-operands))))

(define (make-exponentiation base exponent)
  (cond ((or (=number? base 1) (=number? exponent 0)) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        ((not (number? exponent)) 
         (error "exponent must be a number" exp))
        ((and (number? exponent) (number? base)) (expt base number))
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (car e))

(define (exponent e) (cadr e))

; new derivative.
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; Derivative routines for product and sum.
(define (deriv-product p-operands var)
  (make-sum
   (make-product (multiplier p-operands)
		 (deriv (multiplicand p-operands) var))
   (make-product (deriv (multiplier p-operands) var)
		 (multiplicand p-operands))))

(define (deriv-sum p-operands var)
  (make-sum (deriv (addend p-operands) var)
	    (deriv (augend p-operands) var)))

; Install the derivative routines
(put 'deriv '* deriv-product)
(put 'deriv '+ deriv-sum)

; Add another for exponentiation
(define (deriv-exp p-operands var)
  (make-product 
   (exponent p-operands)
   (make-product (make-exponentiation 
		  (base p-operands)
		  (- (exponent p-operands) 1))
		 (deriv (base p-operands) var))))

(put 'deriv '** deriv-exp)
