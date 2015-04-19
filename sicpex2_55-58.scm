(define p (list list +))
(define q '(list +))
(define r (list 'list '+))



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
        ((exponentiation? exp)
         (make-product 
          (exponent exp)
          (make-product (make-exponentiation 
                         (base exp)
                         (- (exponent exp) 1))
                        (deriv (base exp) var))))
          (else
           (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; oops - no need to modify make-sum for arbitary argument list! Only augend needs to change!! Got this idea from bill the Lizard.
; Quite a few other people had more complicated solutions like my original idea though...
;(define (make-sum-list a)
;  (cond ((null? a) 0)
;        ((=number? (car a) 0) (make-sum-list (cdr a)))
;        ((null? (cdr a)) (car a))
;        ((=number? (cadr a) 0) (car a))
;        ((and (number? (car a)) (number? (cadr a))) 
;         (make-sum-list (cons (+ (car a) (cadr a)) (cddr a))))
;        (else (cons '+ a))))
  
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; similar comment as for make-sum
;(define (make-product-list m)
;  (cond ((null? m) 1)
;        ((=number? (car m) 0) 0)
;        ((null? (cdr m)) (car m))
;        ((=number? (car m) 1) (make-product-list (cdr m)))
;        ((=number? (cadr m) 1) (car m))
;        ((and  (number? (car m)) (number? (cadr m))) 
;         (make-product-list (cons (* (car m) (cadr m)) (cddr m))))
;        (else (cons '* m))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

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

(define (base e) (cadr e))

(define (exponent e) (caddr e))

