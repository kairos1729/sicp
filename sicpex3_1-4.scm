(define (make-accumulator initial)
  (let ((sum initial))
    (lambda (x)
      (set! sum (+ sum x))
      sum)))

(define (make-monitored f)
  (let ((number-of-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) number-of-calls)
	    ((eq? x 'reset-count) (set! number-of-calls 0) 0)
	    (else
	     (set! number-of-calls (+ number-of-calls 1))
	     (f x))))))

(define (make-account balance password)
  (let ((password-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch try-password method)
      (cond ((not (eq? try-password password))
	     (set! password-attempts (+ password-attempts 1))
	     (if (> password-attempts 7)
		 (call-the-cops)
		 (error "Incorrect password")))
	    (else
	     (set! password-attempts 0)
	     (cond
	      ((eq? method 'withdraw) withdraw)
	      ((eq? method 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT" 
			   method))))))
    dispatch))

(define (call-the-cops)
  (error "cops on the way")
  (newline))

