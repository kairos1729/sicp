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
     (display "  ##") 
     (user-print x) 
     (display "##")
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
        (else (actual-value (first-exp exps) env)
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

(define (eval-quoted exp env) 
  (let ((text (text-of-quotation exp)))
    (if (pair? text)
	(let ((a (car text))
	      (b (cdr text)))
	  (eval (list 'cons (list 'quote a) (list 'quote b)) env))
	text)))

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

;;;; make-unbound
(define (eval-make-unbound! exp env)
  (undefine-variable! (cadr exp) env)
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
  (if (true? (actual-value (if-predicate exp) env))
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

;;;; let
;(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-let exp)
  (cons 'let (cddr exp)))

(define (let-bindings exp) (cadr exp))

(define (let-vars exp) 
  (map car (let-bindings exp)))

(define (let-exps exp) 
  (map cadr (let-bindings exp)))

(define (let-body exp) (cddr exp))

(define (let->lambda exp)
  (make-lambda (let-vars exp) (let-body exp)))

(define (let->combination exp)
  (cond ((named-let? exp) 
	 (sequence->exp (list
			 (list 'define 
			       (named-let-name exp) 
			       (let->lambda (named-let-let exp)))
			 (cons (named-let-name exp) 
			       (let-exps (named-let-let exp))))))
	(else (cons (let->lambda exp) (let-exps exp)))))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

;;;; let*
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (let*->nested-lets exp)
  (define (let*->nested-lets-iter bindings body)
    (cond ((null? (cdr bindings))
	   (make-let bindings body))
	  (else
	   (make-let (list (car bindings))
		     (list (let*->nested-lets-iter (cdr bindings) body))))))
  (let*->nested-lets-iter (let-bindings exp) (let-body exp)))

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

;;;; while
;; e.g. (while (< x 5) (set! x (+ 1 x)))
(define (while-cond exp) (cadr exp))

(define (while-body exp) (cddr exp))

(define (expand-while exp)
  (let ((cond-lambda (make-lambda '() (list (while-cond exp))))
	(body-lambda (make-lambda '() (while-body exp)))
	(loop-body (sequence->exp '((body) (l)))))
    (let ((loop-if (make-if '(c) loop-body 'false)))
      (make-let (list (list 'c cond-lambda) 
		      (list 'body body-lambda))
		(list (sequence->exp
		       (list
			(list 'define '(l) loop-if)
			'(l))))))))

(define (eval-while exp env)
  (eval (expand-while exp) env))

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

(define (procedure-parameter-names p) 
  (map (lambda (x) (if (list? x) (car x) x)) (procedure-parameters p)))

(define (procedure-parameter-types p) 
  (map 
   (lambda (x) (if (list? x) (cadr x) 'scheme-standard)) 
   (procedure-parameters p)))

(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-binding name value)
  (cons name value))

(define (binding-name binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))

(define (make-frame variables values)
  (cons 'frame
	(map (lambda (name value) (make-binding name value)) 
	     variables 
	     values)))

(define (frame-bindings frame)
  (cdr frame))

(define (set-frame-bindings! frame bindings)
  (set-cdr! frame bindings))

(define (add-binding-to-frame! var val frame)
  (set-frame-bindings! 
   frame 
   (cons (make-binding var val) 
	 (frame-bindings frame))))

(define (remove-binding-from-frame! var frame)
  (define (scan prev bindings)
    (if (null? bindings)
        (error "Unbound variable" var)	
	(let ((binding (car bindings)))
	  (if (eq? var (binding-name binding))
	      (set-cdr! prev (cdr bindings))
	      (scan bindings (cdr bindings))))))
  (scan frame (frame-bindings frame)))

(define (find-frame-binding frame var)
  (define (scan bindings)
    (if (null? bindings)
	'()
	(let ((binding (car bindings)))
	  (if (eq? var (binding-name binding))
	      binding
	      (scan (cdr bindings))))))
  (scan (frame-bindings frame)))

(define (frame-do-on-binding frame var on-not-found on-found)
  (let ((binding (find-frame-binding frame var)))
    (if (null? binding)
	(on-not-found frame)
	(on-found binding))))

(define (env-do-on-binding env var on-found)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
	(frame-do-on-binding 
	 (first-frame env)
	 var
	 (lambda (frame) (env-loop (enclosing-environment env)))
	 (lambda (binding) (on-found binding)))))
  (env-loop env))
  
;; Interface functions
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (env-do-on-binding 
   env 
   var 
   (lambda (binding) (binding-value binding))))

(define (set-variable-value! var val env)
  (env-do-on-binding 
   env 
   var 
   (lambda (binding) (set-binding-value! binding val))))

(define (define-variable! var val env)
  (frame-do-on-binding 
   (first-frame env)
   var
   (lambda (frame) (add-binding-to-frame! var val frame))
   (lambda (binding) (set-binding-value! binding val))))

(define (undefine-variable! var env)
  (remove-binding-from-frame! var (first-frame env)))


;; Global environment
;; 4.32
(define (wastetime n)
  (if (= n 0) 1 (wastetime (- n 1))))

(define (delay-and-return n)
  (wastetime 1000000)
  n)

(define (longcalcstream n)
  (if (= n 0)
      '()
      (cons-stream (delay-and-return n) (longcalcstream (- n 1)))))

(define (install-better-stream-example)
  (eval 
   '(begin
      (define (longcalcstream (n lazy-memo))
	(if (= n 0)
	    '()
	    (cons (delay-and-return n) (longcalcstream (- n 1)))))
      )
   the-global-environment))

;;;; Primitive procedures
(define primitive-procedures
  (list (list 'primitive-car car)
        (list 'primitive-cdr cdr)
        (list 'primitive-cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'not not)
        (list 'assoc assoc)
	;; cadr and list don't work with our lazy lists
        ;;(list 'cadr cadr)
        ;;(list 'list list)
        (list 'wastetime wastetime)
        (list 'delay-and-return delay-and-return)
	;; map doesn't work as it doesn't take our primitives as arguments
        ;;(list 'map map)
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

;; Thunks
(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
	 (d-trace "Force: ")
	 (up-trace obj)
	 (nl-trace)
	 (actual-value (thunk-exp obj) (thunk-env obj)))
	((thunk-memo? obj)
	 (d-trace "Force and memoize: ")
	 (up-trace obj)
	 (nl-trace)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result) ; replace exp with its value
	   (set-cdr! (cdr obj) '()) ; forget unneeded env
	   result))
	((evaluated-thunk? obj)
	 (d-trace "Memoized: ")
	 (up-trace obj)
	 (nl-trace)
	 (thunk-value obj))
	(else obj)))

(define (actual-value exp env)
  (force-it (eval exp env)))

;; Lazy lists
;; ex 4.43 took approach of http://community.schemewiki.org/?sicp-ex-4.34
;; Kind of thought of this myself, but wasn't confidednt to give it a go
(define (install-lazy-lists)
  (eval 
   '(begin
      (define (cons (x lazy-memo) (y lazy-memo))
	(primitive-cons 'lazy-pair (lambda ((m lazy-memo)) (m x y))))

      (define (car (z lazy-memo))
	((primitive-cdr z) (lambda ((p lazy-memo) (q lazy-memo)) p)))

      (define (cdr (z lazy-memo))
	((primitive-cdr z) (lambda ((p lazy-memo) (q lazy-memo)) q)))

      (define (list-ref (items lazy-memo) (n lazy-memo))
	(if (= n 0)
	    (car items)
	    (list-ref (cdr items) (- n 1))))
      
      (define (map (proc lazy-memo) (items lazy-memo))
	(if (null? items)
	    '()
	    (cons (proc (car items))
		  (map proc (cdr items)))))
      
      (define (scale-list (items lazy-memo) (factor lazy-memo))
	(map (lambda (x) (* x factor))
	     items))
      
      (define (add-lists (list1 lazy-memo) (list2 lazy-memo))
	(cond ((null? list1) list2)
	      ((null? list2) list1)
	      (else (cons (+ (car list1) (car list2))
			  (add-lists (cdr list1) (cdr list2))))))
      
      (define ones (cons 1 ones))
      
      (define integers (cons 1 (add-lists ones integers)))

      (define (integral 
	       (integrand lazy-memo) 
	       (initial-value lazy-memo) 
	       (dt lazy-memo))
	(define int
	  (cons initial-value
		(add-lists (scale-list integrand dt)
			   int)))
	int)
      
      (define (solve (f lazy-memo) (y0 lazy-memo) (dt lazy-memo))
	(define y (integral dy y0 dt))
	(define dy (map f y))
	y)
      )
   the-global-environment))
	   
(define (eval-lazy-pair exp env)
  exp)

;; REPL
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (set! indent "")
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object-outer)
  (define (user-print-list object)
    (user-print (car object))
    (if (not (null? (cdr object)))
	(begin
	  (display " ")
	  (user-print-internal (cdr object)))))

  (define (user-print-lazy-list object n)
    (let ((original-trace trace?))
      (set! trace? false)
      (if (< n 1)
	  (display "...")
	  (begin
	    (user-print 
	     (actual-value (list 'car object) the-global-environment))
	    (let ((x (actual-value (list 'cdr object) the-global-environment)))
	      (if (not (null? x))
		  (begin
		    (display " ")
		    (if (lazy-pair? x)
			(user-print-lazy-list x (- n 1))
			(begin
			  (display ". ")
			  (user-print x))))))))
      (set! trace? original-trace)))

  (define (user-print-internal object)
    (cond ((compound-procedure? object)
	   (user-print-internal (list 'compound-procedure
				      (procedure-parameters object)
				      (procedure-body object)
				      '<procedure-env>)))
	  ((thunk? object)
	   (user-print-internal (list (thunk-exp object) '<thunk-env>)))
	  ((thunk-memo? object)
	   (user-print-internal (list (thunk-exp object) '<thunk-memo-env>)))
	  ((lazy-pair? object) (user-print-lazy-list object list-limit))
	  ((list? object) (if (not (null? object)) (user-print-list object)))
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

(define (lazy-pair? x)
  (and (tagged-list? x 'lazy-pair) (compound-procedure? (cdr x))))

(define list-limit 50)

(define (restart)
  (set! the-global-environment (setup-environment))
  (driver-loop))

; Eval/Apply:
(define (apply_mc procedure arguments env)
  (d-trace "Apply: ")
  (up-trace procedure)
  (nl-trace)
  (d-trace "On:    ")
  (up-trace arguments)
  (nl-trace)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
	  procedure 
	  (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameter-names procedure)
	   (list-of-delayed-args 
	    (procedure-parameter-types procedure)
	    arguments 
	    env)
	   (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps)
				env))))

(define (list-of-delayed-args types exps env)
  (if (no-operands? exps)
      '()
      (let ((type (car types))
	    (operand (first-operand exps)))
	(let ((delayed-exp
	       (cond ((eq? type 'scheme-standard) (actual-value operand env))
		     ((eq? type 'lazy-memo) (delay-it-memo operand env))
		     ((eq? type 'lazy) (delay-it operand env))
		     (else (error "Unknown argument type" type)))))
	  (cons delayed-exp (list-of-delayed-args 
			     (cdr types)
			     (rest-operands exps)
			     env))))))

(define (eval exp env)
  (incindent)
  (d-trace "Eval:  ")
  (up-trace exp)
  (cond ((self-evaluating? exp) 
	 (cdnl-trace "self-evaluating") 
	 (return-trace exp))
        ((variable? exp) 
	 (let ((val (lookup-variable-value exp env)))
	   (cdnl-trace (list "variable" val)) 
	   (return-trace val)))
        ((true? (get (car exp))) 
	 (cdnl-trace (car exp)) 
	 (return-trace ((get (car exp)) exp env)))
        ((application? exp)
	 (cdnl-trace "application") 
	 (return-trace 	  
	  (apply_mc (actual-value (operator exp) env)
		    (operands exp) 
		    env)))
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
(put 'let eval-let)
(put 'let* eval-let*)
(put 'while eval-while)
(put 'make-unbound! eval-make-unbound!)

;; Need this so we can print lists
(put 'lazy-pair eval-lazy-pair)

