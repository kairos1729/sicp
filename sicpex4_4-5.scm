;; Eval/Apply
(define *op-table* (make-hash-table))

(define (put expression-type proc)
  (hash-table/put! *op-table* expression-type proc))

(define (get expression-type)
  (hash-table/get *op-table* expression-type false))

(define trace? true)

(define indent "")

(define (incindent)
  (iftrace (lambda() (set! indent (string-append indent " ")))))

(define (decindent)
  (iftrace (lambda () (set! indent (string-tail indent 1)))))

(define (iftrace f)
  (if trace? (f)))

(define (d-trace x)
  (iftrace (lambda() (display indent) (display x))))

(define (up-trace x)
  (iftrace (lambda() (user-print x))))

(define (nl-trace)
  (iftrace (lambda() (newline))))

(define (cdnl-trace x)
  (iftrace 
   (lambda() 
     (display " **") 
     (display x) 
     (display "**")
     (newline))))

(define (return-trace x) (decindent) x)

;; Eval: procedure arguments, if, sequence, assignment, definition

; ex 4.1
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (cons (eval (first-operand exps) env)
;            (list-of-values (rest-operands exps) env))))

; force left-to-right version
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;     '()
;      (let ((left (eval (first-operand exps) env)))
;	(cons left (list-of-values (rest-operands exps) env)))))

; force right-to-left version
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env) right))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Representations
;;;; Primitive
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;;; Variable
(define (variable? exp) (symbol? exp))

;;;; Application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;; Tagged items
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;; Quoted
;(define (quoted? exp)
;  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (eval-quoted exp env) (text-of-quotation exp))

;;;; Assignment
;(define (assignment? exp)
;  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;;; Define
;(define (definition? exp)
;  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;; Lambda
;(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (eval-lambda exp env) 
  (make-procedure (lambda-parameters exp)
		  (lambda-body exp)
		  env))

;;;; If
;(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;;; Begin
;(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (eval-begin exp env) 
  (eval-sequence (begin-actions exp) env))

;;;; Cond
;(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-test clause) (car clause))
(define (cond-recipient-actions clause) (cddr clause))
(define (cond-test-clause? clause)
  (eq? (cadr clause) '=>))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; (define (expand-clauses clauses)
;;   (if (null? clauses)
;;       'false                          ; no else clause
;;       (let ((first (car clauses))
;;             (rest (cdr clauses)))
;;         (if (cond-else-clause? first)
;;             (if (null? rest)
;;                 (sequence->exp (cond-actions first))
;;                 (error "ELSE clause isn't last -- COND->IF"
;;                        clauses))
;;             (make-if (cond-predicate first)
;;                      (sequence->exp (cond-actions first))
;;                      (expand-clauses rest))))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND->IF"
			  clauses)))
	      ((cond-test-clause? first)
	       (list
		(make-lambda '(test-exp recipient-exp rest-of-expanded-cond)
			     (list (make-if 'test-exp
					    '((recipient-exp) test-exp)
					    '(rest-of-expanded-cond))))
		(cond-test first)
		(make-lambda '() (list (sequence->exp (cond-recipient-actions first))))
		(make-lambda '() (list (expand-clauses rest)))))
	      (else
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))

(define (eval-cond exp env)
  (eval (cond->if exp) env))

;;;; and
(define (make-and exps)
  (cons 'and exps))

(define (and-exps exp)
  (cdr exp))

(define (eval-and-exps exps env)
  (if (null? exps)
      true
      (let ((val (eval (car exps) env)))
	(if (false? val)
	    false
	    (if (null? (cdr exps))
		val
		(eval-and-exps (cdr exps) env))))))

(define (expand-and-exps exps)
  (if (null? exps)
      'true
      (if (null? (cdr exps))
	  (car exps)
	  (make-if (car exps) (expand-and-exps (cdr exps)) 'false))))

;; This is the version I think is suggested in the manual, although in the
;; case above I don't believe there are name conflicts, since I don't 
;; need to use a let, since I return 'false instead of (car exps) in the if.
;; I wonder if this is not correct (maybe stuff other than 'false evaluates 
;; to false?? Although 4.1.3 says this is OK.
(define (expand-and-exps-no-name-conflict exps)
  (cond ((null? exps) 'true)
	((null? (cdr exps)) (car exps))
	(else
	 (list (make-lambda 
		'(first-exp rest-of-and) 
		(list (make-if 'first-exp '(rest-of-and) 'first-exp)))
	       (car exps)
	       (make-lambda '() (list (make-and (cdr exps))))))))

(define (eval-and exp env)
  (eval-and-exps (and-exps exp) env))
;;  (eval (expand-and-exps (and-exps exp)) env))
;;  (eval (expand-and-exps-no-name-conflict (and-exps exp)) env))

;;;; or
(define (make-or exps)
  (cons 'or exps))

(define (or-exps exp)
  (cdr exp))

(define (eval-or-exps exps env)
  (if (null? exps)
      false
      (let ((val (eval (car exps) env)))
	(if (true? val)
	    val
	    (eval-or-exps (cdr exps) env)))))

;; This uses a let and has name conflict problems
;; also, we could recurse on or
(define (expand-or-exps exps)
  (if (null? exps)
      'false
      (if (null? (cdr exps))
	  (car exps)
	  (list 
	   (make-lambda '(x) 
			(list
			 (make-if 'x
				  'x 
				  ;; oops - potential name conflict if (cdr exps)
				  ;; refer to x in an outer scope :(
				  (expand-or-exps (cdr exps)))))
	   (car exps)))))

;; This is the version I think is suggested in the manual
(define (expand-or-exps-no-name-conflict exps)
  (if (null? exps)
      'false
      (list (make-lambda '(first-exp rest-of-or) 
			 (list (make-if 'first-exp 'first-exp '(rest-of-or))))
	    (car exps)
	    (make-lambda '() (list (make-or (cdr exps)))))))

(define (eval-or exp env)
  (eval-or-exps (or-exps exp) env))
;;  (eval (expand-or-exps (or-exps exp)) env))
;;  (eval (expand-or-exps-no-name-conflict (or-exps exp)) env))

;; Eval data structures: Predicates, procedures, environments
;;;; Predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;;; Procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Global environment
;;;; Primitive procedures
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'not not)
        (list 'assoc assoc)
        (list 'cadr cadr)
        (list 'list list)
        ;;<more primitives>
        ))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

; see footnote 17 p382.  I've just renamed the metacircular apply, it's easier
; than trying to remember to save the scheme apply.  
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;; Setup
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; REPL
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (set! indent "")
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object-outer)
  (define (user-print-internal object)
    (cond ((compound-procedure? object)
	   (user-print-internal (list 'compound-procedure
				      (procedure-parameters object)
				      (procedure-body object)
				      '<procedure-env>)))
	  ((list? object) 
	   (if (not (null? object))
	       (begin
		 (user-print (car object))
		 (if (not (null? (cdr object)))
		     (begin
		       (display " ")
		       (user-print-internal (cdr object)))))))
	  (else (display object))))
  (if (list? object-outer) (display "("))
  (user-print-internal object-outer)
  (if (list? object-outer) (display ")")))

;; (define (user-print object)
;;   (if (compound-procedure? object)
;;       (display (list 'compound-procedure
;;                      (procedure-parameters object)
;;                      (procedure-body object)
;;                      '<procedure-env>))
;;       (display object)))

(define (restart)
  (set! the-global-environment (setup-environment))
  (driver-loop))

; Eval/Apply:
(define (apply_mc procedure arguments)
  (d-trace "Apply: ")
  (up-trace procedure)
  (nl-trace)
  (d-trace "On:    ")
  (up-trace arguments)
  (nl-trace)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (eval exp env)
  (incindent)
  (d-trace "Eval:  ")
  (up-trace exp)
  (cond ((self-evaluating? exp) 
	 (cdnl-trace "self-evaluating") 
	 (return-trace exp))
        ((variable? exp) 
	 (cdnl-trace "variable" ) 
	 (return-trace (lookup-variable-value exp env)))
        ((true? (get (car exp))) 
	 (cdnl-trace (car exp)) 
	 (return-trace ((get (car exp)) exp env)))
        ((application? exp)
	 (cdnl-trace "application") 
	 (return-trace 	  
	  (apply_mc (eval (operator exp) env)
		    (list-of-values (operands exp) env))))
        (else
         (error "Unknown expression type -- EVAL" exp)
	 (decindent))))

(put 'quote eval-quoted)
(put 'set! eval-assignment)
(put 'define eval-definition)
(put 'lambda eval-lambda)
(put 'if eval-if)
(put 'begin eval-begin)
(put 'cond eval-cond)
(put 'and eval-and)
(put 'or eval-or)
