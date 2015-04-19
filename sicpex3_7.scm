(define (make-account balance password)
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (make-joint joint-account-password)
      (lambda (try-password method)
	(dispatch joint-account-password try-password method)))
    (define (dispatch account-password try-password method)
      (cond ((not (eq? try-password account-password)) (error "Incorrect password"))
	    ((eq? method 'withdraw) withdraw)
	    ((eq? method 'deposit) deposit)
	    ((eq? method 'make-joint) make-joint)
	    (else (error "Unknown request -- MAKE-ACCOUNT" 
			 method))))
    (lambda (try-password method) 
      (dispatch password try-password method)))

(define (make-joint account account-password joint-password)
  ((account account-password 'make-joint) joint-password))

