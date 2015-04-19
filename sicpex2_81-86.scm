(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) false))

(define (put-coercion t1 t2 proc)
  (hash-table/put! *op-table* (list 'coerce t1 t2) proc))

(define (get-coercion t1 t2)
  (hash-table/get *op-table* (list 'coerce t1 t2) false))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;;;SECTION 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) 
	 (if (integer? x)
	  (tag x)
	  (error x " is not an integer"))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'project '(integer) (lambda (x) (tag x)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) 
	 (if (real? x)
	  (tag x)
	  (error x " is not a a real number"))))
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real)
       (lambda (x) (make-integer (round x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
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
  ;; interface to rest of the system
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
       (lambda (n d) 
	 (if (and (integer? n) (integer? d))
	  (tag (make-rat n d))
	  (error n " and " d " are not both integers"))))
  (put 'equ? '(rational rational)
       (lambda (x y) 
	 (and (= (numer x) (numer y)) 
	      (= (denom x) (denom y)))))
  (put 'raise '(rational)
       (lambda (x) 
	 (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational)
       (lambda (x) 
	 (make-integer 
	  (round 
	   (/ (numer x) (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (x y) 
	 (and (= (real-part x) (real-part y)) 
	      (= (imag-part x) (imag-part y)))))
  (put 'raise '(complex) (lambda (x) (tag x)))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-complex-package)
(install-rational-package)
(install-integer-package)
(install-real-package)



;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;            "No method for these types -- APPLY-GENERIC"
;            (list op type-tags))))))
(define (apply-generic op . args)
  (dpgtrace "generic ")
  (dpgtrace op)
  (dpgtrace ":")
  (dpgtrace args)
  (dpgnewline)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (drop (apply proc (map contents args)))
	  (if (= (length args) 2)
	      (let ((a1 (car args))
		    (a2 (cadr args)))
		(let ((level1 (level a1))
		      (level2 (level a2)))
		  (cond ((> level2 level1)
			 (apply-generic op (raise a1) a2))
			((> level1 level2)
			 (apply-generic op a1 (raise a2)))
			(else (error "No method for these types 1"
				     (list op type-tags))))))
	      (error "No method for these types 3"
		     (list op type-tags)))))))

(define (scheme-number->rational n)
  (make-rational (contents n) 1))
(put-coercion 'scheme-number 'rational scheme-number->rational)

; 2.81, but not needed
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;	      scheme-number->scheme-number)
;(put-coercion 'complex 'complex 
;	      complex->complex)


(define (level x)
  (define (level-iter t n)
    (let ((raised (raise t)))
      (if (eq? (type-tag raised)  (type-tag t))
	  n
	  (level-iter raised (- n 1)))))
  (level-iter x 0))

; don't define project or raise via apply generic as we get into infinite recursion 
; if we use drop and also if we try to drop after raise that won't work 
; also, anything without a tag or that has no project op just return the value
(define (raise x)
  (dpgtrace "raise: ")
  (dpgtrace x)
  (dpgnewline)
  (if (not (pair? x))
      x
      (let ((raise-op (get 'raise (list (type-tag x)))))
	(if (not raise-op)
	    x
	    (raise-op (contents x))
	    ))))
(define (get-project-op x)
  (if (pair? x)
      (get 'project (list (type-tag x)))
      false))

(define (can-project x)
  (dpgtrace "can project: ")
  (dpgtrace x)
  (dpgnewline)
  (not (eq? false (get-project-op x))))

(define (project x)
  (dpgtrace "project: ")
  (dpgtrace x)
  (dpgnewline)
  (let ((project-op (get-project-op  x)))
    (if (not project-op)
	x
	(project-op (contents x))
	)))

(define (drop x)
  (dpgtrace "drop: ")
  (dpgtrace x)
  (dpgtrace ", ")
  (dpgtrace (pair? x))
  (dpgnewline)
  (if (not (can-project x))
      x
      (let ((p (project x)))
	(dpgtrace "p: ")
	(dpgtrace p)
	(dpgtrace " x: ")
	(dpgtrace x)
	(dpgnewline)
	(if (eq? (type-tag p) (type-tag x))
	    x
	    (let ((r (raise p)))
	      (if (equ? r x)
		  (drop p)
		  x))))))



