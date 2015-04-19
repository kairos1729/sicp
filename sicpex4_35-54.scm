(define apply-in-underlying-scheme apply)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)  (cons 'lambda (cons parameters body)))

(define (require? exp) (tagged-list? exp 'require))

(define (require-predicate exp) (cadr exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-main exp) (cadr exp))

(define (if-fail-alternative exp) (caddr exp))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
;;;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))

(define (let-vars exp) 
  (map car (let-bindings exp)))

(define (let-exps exp) 
  (map cadr (let-bindings exp)))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) 
	(let-exps exp)))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


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

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; for the queens problem (my own experiments) ex 4.44
(define (record-try)
  (set! positions-tried (+ 1 positions-tried)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cddr cddr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'list list)
        (list 'list-head list-head)
        (list 'list-tail list-tail)
        (list 'length length)
        (list 'append append)
	(list 'memq memq)
        (list 'null? null?)
        (list 'not not)
	(list 'eq? eq?)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '- -)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list 'even? even?)
        (list 'odd? odd?)
	(list 'round round)
        (list 'remainder remainder)
        (list 'member member)
        (list 'abs abs)
        (list 'display display)
	(list 'newline newline)
	(list 'record-try record-try)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	  (try-again)
	  (begin
	    (newline)
	    (display ";;; Starting a new problem ")
	    (ambeval input
		     the-global-environment
		     ;; ambeval success
		     (lambda (val next-alternative)
		       (announce-output output-prompt)
		       (user-print val)
		       (internal-loop next-alternative))
		     ;; ambeval failure
		     (lambda ()
		       (announce-output
			";;; There are no more values of")
		       (user-print input)
		       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))
     
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (loadeval)
  (set! the-global-environment (setup-environment))
  (install-example-procs)
  (driver-loop))

;; Analysing stuff here
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
;;  (display "az ")
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((require? exp) (analyze-require exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
	((amb? exp) (analyze-amb exp))
	((ramb? exp) (analyze-ramb exp))
	((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;; Amb
(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    ((car choices) 
	     env
	     succeed
	     (lambda ()
	       (try-next (cdr choices))))))
      (try-next cprocs))))

;; Ramb
(define (ramb? exp)
  (tagged-list? exp 'ramb))

(define (ramb-choices exp) (cdr exp))

(define next-ramb-id
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      id)))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp)))
	(id (next-ramb-id)))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    (let ((index (random (length choices))))
	      (let ((chosen (list-ref choices index))
		    (rest (append (list-head choices index) 
				  (list-tail choices (+ index 1)))))
		(display "id: ")
		(display id)
		(display " index chosen: ")
		(display index)
		(newline)
		(chosen
		 env
		 succeed
		 (lambda () (try-next rest)))))))
      (try-next cprocs))))
	     
;; Other stuff
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) 
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) 
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) 
	     fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (let ((old-value
		      (lookup-variable-value var env)))
		 (set-variable-value! var val env)
		 (succeed 'ok
			  (lambda ()
			    (set-variable-value! var
						 old-value
						 env)
			    (fail2)))))
	     fail))))

;; Ex 4.51
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     ;; success continuation for evaluating the predicate
	     ;; to obtain pred-value
	     (lambda (pred-value fail2)
	       (if (true? pred-value)
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     ;; failure continuation for evaluating the predicate
	     fail))))

(define (analyze-if-fail exp)
  (let ((mproc (analyze (if-fail-main exp)))
        (fproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (mproc env	     
	     succeed
	     (lambda () (fproc env succeed fail))))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not pred-value)
		   (fail2)
		   (succeed 'ok fail2)))
	     fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail) 
      (succeed (make-procedure vars bproc env)
	       fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail) 
      (proc1 env 
	     ;; success continuation for calling a
	     (lambda (a-value fail2) 
	       (proc2 env succeed fail2))
	     fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc fail2)
	       (get-args aprocs
			 env
			 (lambda (args fail3)
			   (execute-application
			    proc args succeed fail3))
			 fail2))
	     fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) 
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
	 (get-args (cdr aprocs)
		   env
		   ;; success continuation for recursive
		   ;; call to get-args
		   (lambda (args fail3)
		     (succeed (cons arg args)
			      fail3))
		   fail2))
       fail)))
       
(define (execute-application proc args succeed fail)
;;  (display "ex ")
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args)
		  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
	  succeed
	  fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (ambevalexp exp)
  (user-print exp)
  (newline)
  (ambeval exp 
	   the-global-environment 
	   (lambda (val fail) (user-print val)) 
	   (lambda () 'fail))
  (newline))

(define (install-example-procs)
  ;;(ambevalexp 
   ;;'(define (require p)
   ;;   (if (not p) (amb))))  
  (ambevalexp 
   '(define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items)))))
  (ambevalexp 
   '(define (a-random-element-of items)
      (require (not (null? items)))
      (let ((number (length items)))
	(require (> number 0))
	(if (= 1 number)
	    (car items)
	    (let ((half (round (/ number 2))))
	      (ramb 
	       (a-random-element-of (list-head items half))
	       (a-random-element-of (list-tail items half))))))))
  (ambevalexp 
   '(define (smallest-divisor n)
      (find-divisor n 2)))
  (ambevalexp 
   '(define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
	    ((divides? test-divisor n) test-divisor)
	    (else (find-divisor n (+ test-divisor 1))))))
  (ambevalexp 
   '(define (square x) (* x x)))
  (ambevalexp
   '(define (divides? a b)
      (= (remainder b a) 0)))
  (ambevalexp
   '(define (prime? n)
      (= n (smallest-divisor n))))
  (ambevalexp
   '(define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
	    (b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b))))
  (ambevalexp
   '(define (an-integer-between lo hi)
      (if (> lo hi)
	  (amb)
	  (amb lo (an-integer-between (+ lo 1) hi)))))
  (ambevalexp
   '(define (an-integer-starting-from lo)
      (amb lo (an-integer-starting-from (+ lo 1)))))
  (ambevalexp
   '(define (a-pythagorean-triple-between lo hi)
      (let ((i (an-integer-between lo hi)))
	(let ((j (an-integer-between i hi)))
	  (let ((k (an-integer-between j hi)))
	    (require (= (+ (square i) (square j)) (square k)))
	    (list i j k))))))
  (ambevalexp
   '(define (a-pythagorean-triple)
      (let ((k (an-integer-starting-from 1)))
	(let ((j (an-integer-between 1 k)))
	  (let ((i (an-integer-between 1 j)))
	    (require (= (+ (square i) (square j)) (square k)))
	    (list i j k))))))
  (ambevalexp
   '(define (distinct? items)
      (cond ((null? items) true)
	    ((null? (cdr items)) true)
	    ((member (car items) (cdr items)) false)
	    (else (distinct? (cdr items))))))
  (ambevalexp
   '(define (multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
	    (cooper (amb 1 2 3 4 5))
	    (fletcher (amb 1 2 3 4 5))
	    (miller (amb 1 2 3 4 5))
	    (smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith)))))
  (ambevalexp
   '(define (multiple-dwelling-4-38)
      (let ((baker (amb 1 2 3 4 5))
	    (cooper (amb 1 2 3 4 5))
	    (fletcher (amb 1 2 3 4 5))
	    (miller (amb 1 2 3 4 5))
	    (smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith)))))
  (ambevalexp
   '(define (multiple-dwelling-4-39-faster)
      (let ((baker (amb 1 2 3 4 5))
	    (cooper (amb 1 2 3 4 5))
	    (fletcher (amb 1 2 3 4 5))
	    (miller (amb 1 2 3 4 5))
	    (smith (amb 1 2 3 4 5)))
	(require (> miller cooper))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith)))))
  (ambevalexp
   '(define (multiple-dwelling-4-40-even-faster)
      (let ((cooper (amb 2 3 4 5))
	    (fletcher (amb 2 3 4)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(let ((smith (amb 1 2 3 4 5)))
	  (require (not (= (abs (- smith fletcher)) 1)))
	  (let ((miller (amb 3 4 5)))
	    (require (> miller cooper))
	    (let ((baker (amb 1 2 3 4)))
	      (require
	       (distinct? (list baker cooper fletcher miller smith)))
	      (list (list 'baker baker)
		    (list 'cooper cooper)
		    (list 'fletcher fletcher)
		    (list 'miller miller)
		    (list 'smith smith))))))))
  ;; Ex 4.42
  (ambevalexp
   '(define (liars)
      (define (bool-to-int b)
	(if b 1 0))
      (define (oneof b1 b2)
	(= 1 (+ (bool-to-int b1) (bool-to-int b2))))
      (let ((b (amb 1 2 3 4 5))
	    (e (amb 1 2 3 4 5))
	    (j (amb 1 2 3 4 5))
	    (k (amb 1 2 3 4 5))
	    (m (amb 1 2 3 4 5)))
	(require (oneof (= k 2) (= b 3)))
	(require (oneof (= e 1) (= j 2)))
	(require (oneof (= j 3) (= e 5)))
	(require (oneof (= k 2) (= m 4)))
	(require (oneof (= m 4) (= 1 b)))
	(require (distinct? (list b e j k m)))
	(list (list 'betty b)
	      (list 'ethel e)
	      (list 'joan j)
	      (list 'kitty k)
	      (list 'mary m)))))

  ;; Ex 4.43
  (ambevalexp
   '(define (daughters)
      (let ((dd (amb 'a 'g 'l 'r 'm))
	    (dh (amb 'a 'g 'l 'r 'm))	    
	    (db (amb 'a 'g 'l 'r 'm))	    
	    (dp (amb 'a 'g 'l 'r 'm))	    
	    (dm (amb 'a 'g 'l 'r 'm))	    
	    (yd (amb 'a 'g 'l 'r 'm))	    
	    (yh (amb 'a 'g 'l 'r 'm))	    
	    (yb (amb 'a 'g 'l 'r 'm))	    
	    (yp (amb 'a 'g 'l 'r 'm))	    
	    (ym (amb 'a 'g 'l 'r 'm)))
	(require (eq? dm 'a))
	(require (eq? db 'm))
	(require (eq? yb 'g))
	(require (eq? ym 'l))
	(require (eq? yh 'r))
	(require (eq? yd 'm))
	(require (or (and (eq? yd dp) (eq? dd 'g))
		     (and (eq? yh dp) (eq? dh 'g))
		     (and (eq? yb dp) (eq? db 'g))
		     (and (eq? ym dp) (eq? dm 'g))))
	(require (not (eq? dd yd)))
	(require (not (eq? dh yh)))
	(require (not (eq? db yb)))
	(require (not (eq? dp yp)))
	(require (not (eq? dm ym)))
	(require (distinct? (list dd dh db dp dm)))
	(require (distinct? (list yd yh yb yp ym)))
	(list (list 'downing dd)
	      (list 'hall dh)
	      (list 'barnacle db)
	      (list 'parker dp)
	      (list 'moore dm)))))
  (ambevalexp
   '(define (daughters-faster)
      (let ((yh 'r)	    
	    (yb 'g)
	    (ym 'l)	    
	    (yd 'm)	    
	    (yp 'a)
	    (db 'm)	    
	    (dm 'a)	    
	    (dd (amb 'g 'l 'r))
	    (dh (amb 'g 'l))	    
	    (dp (amb 'l 'r)))
	(require (cond ((eq? dd 'g) (eq? yd dp))
		       ((eq? dh 'g) (eq? yh dp))
		       (else false)))
	(require (not (eq? dd yd)))
	(require (not (eq? dh yh)))
	(require (not (eq? db yb)))
	(require (not (eq? dp yp)))
	(require (not (eq? dm ym)))
	(require (distinct? (list dd dh db dp dm)))
	(require (distinct? (list yd yh yb yp ym)))
	(list (list 'downing dd)
	      (list 'hall dh)
	      (list 'barnacle db)
	      (list 'parker dp)
	      (list 'moore dm)))))
  (ambevalexp
   '(define (daughters-faster-no-moore)
      (let ((yh 'r)
	    (yb 'g)
	    (ym 'l)	    
	    (yd 'm)	    
	    (yp 'a)
	    (db 'm)	    
	    (dd (amb 'a 'g 'l 'r))
	    (dh (amb 'a 'g 'l))	    
	    (dp (amb 'l 'r))	    
	    (dm (amb 'a 'g 'r)))
	(require (cond ((eq? dd 'g) (eq? yd dp))
		       ((eq? dh 'g) (eq? yh dp) )
		       ((eq? dm 'g) (eq? ym dp))
		       (else false)))
	(require (distinct? (list dd dh db dp dm)))
	(require (distinct? (list yd yh yb yp ym)))
	(list (list 'downing dd)
	      (list 'hall dh)
	      (list 'barnacle db)
	      (list 'parker dp)
	      (list 'moore dm)))))
  ;; Ex 4.44
  (ambevalexp
   '(define (queens)
      (define (addqueen positions)
	(if (> (length positions) 7)
	    positions
	    (let ((new-row (amb 1 2 3 4 5 6 7 8)))
	      (let ((new-positions (append (list new-row) positions)))
		(record-try)
		(require (safe? new-positions))
		(addqueen new-positions)))))
      (addqueen '())))

  (ambevalexp
   '(define empty-board (list)))
  
  (ambevalexp
   '(define (safe-diags? positions)
      (define (safe-diags-iter? dist row rest-of-rows)
	(if (null? rest-of-rows)
	    (begin
	      true)
	    (let ((illegal-row1 (+ (car rest-of-rows) dist))
		  (illegal-row2 (- (car rest-of-rows) dist)))
	      (if (= illegal-row1 row) 
		  false
		  (if (= illegal-row2 row)
		      false
		      (safe-diags-iter? (+ dist 1) row (cdr rest-of-rows)))))))
      (safe-diags-iter? 1 (car positions) (cdr positions))))
  
  (ambevalexp
   '(define (safe-rows? positions)
	(not (memq (car positions) (cdr positions)))))
  
  (ambevalexp
   '(define (safe? positions)
      (if (not (safe-diags? positions))
	  false
	  (if (not (safe-rows? positions))
	      false
	      true))))

  ;; Section 4.3.2
  (ambevalexp
   '(begin
      (define nouns '(noun student professor cat class))
      (define verbs '(verb studies lectures eats sleeps))
      (define articles '(article the a))
      
      ;; (define (parse-word word-list)
      ;; 	(require (not (null? *unparsed*)))
      ;; 	(require (memq (car *unparsed*) (cdr word-list)))
      ;; 	(let ((found-word (car *unparsed*)))
      ;; 	  (set! *unparsed* (cdr *unparsed*))
      ;; 	  (list (car word-list) found-word)))
      
      (define (parse-word word-list)
	(a-random-element-of(cdr word-list)))

      (define *unparsed* '())
      
      (define (parse input)
	(set! *unparsed* input)
	(let ((sent (parse-sentence)))
	  (require (null? *unparsed*))
	  sent))
      
      (define prepositions '(prep for to in by with))
      
      (define adjectives '(adjective big small long short loud soft))

      (define adverbs '(adverb loudly softly quickly slowly))
      
      (define (parse-prepositional-phrase)
	(list 'prep-phrase
	      (parse-word prepositions)
	      (parse-noun-phrase)))
      
      (define (parse-sentence)
	(list 'sentence
	      (parse-noun-phrase)
	      (parse-verb-phrase)))
      
      (define (parse-verb-phrase)
      	(define (maybe-extend verb-phrase)
      	  (ramb verb-phrase
      	       (maybe-extend (list 'verb-phrase1
      				   verb-phrase
      				   (parse-prepositional-phrase)))
	       (maybe-extend (list 'verb-phrase2
				   verb-phrase
				   (parse-word adverbs)))))
	(maybe-extend (ramb (parse-word verbs) 
			   (list 'verb-phrase4 
				 (parse-word adverbs)
				 (parse-word verbs)))))


      ;; (define (parse-verb-phrase)
      ;; 	(amb (list 'verb-phrase
      ;; 		   (parse-verb-phrase)
      ;; 		   (parse-prepositional-phrase))
      ;; 	     (parse-word verbs)))
      
      (define (parse-qualified-noun)
	(ramb (parse-word nouns)
	     (list 'qualified-noun
		   (parse-word adjectives)
		   (parse-qualified-noun))))

      (define (parse-simple-noun-phrase)
	(list 'simple-noun-phrase
	      (parse-word articles)
	      (parse-qualified-noun)))
      
      (define (parse-noun-phrase)
	(define (maybe-extend noun-phrase)
	  (ramb noun-phrase
	       (maybe-extend (list 'noun-phrase
				   noun-phrase
				   (parse-prepositional-phrase)))))
	(maybe-extend (parse-simple-noun-phrase)))
      ))
  ;; Ex 4.51
  (ambevalexp
   '(begin
     (define count 0)
     (define (test1)
       (let ((x (an-element-of '(a b c)))
	     (y (an-element-of '(a b c))))
	 (permanent-set! count (+ count 1))
	 (require (not (eq? x y)))
	 (list x y count)))
     (define (test2)
       (let ((x (an-element-of '(a b c)))
	     (y (an-element-of '(a b c))))
	 (set! count (+ count 1))
	 (require (not (eq? x y)))
	 (list x y count)))))
  ;; Ex 4.52
  (ambevalexp
   '(begin
     (define (test-if-fail1)
       (if-fail 
	(let ((x (an-element-of '(1 3 5))))
	  (require (even? x))
	  x)
	'all-odd))
     (define (test-if-fail2)
       (if-fail 
	(let ((x (an-element-of '(1 3 5 8 11 12 15 18 17 19))))
	  (require (even? x))
	  x)
	'all-odd))))
  ;; Ex 4.53
  (ambevalexp
   '(define (test-pairs)
      (let ((pairs '()))
	(if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
		   (permanent-set! pairs (cons p pairs))
		   (amb))
		 pairs))))
  )

(define positions-tried 0)

(define (find-queens)
  (set! positions-tried 0)
  (ambeval '(queens)
	   the-global-environment
	   ;; ambeval success
	   (lambda (val next-alternative)
	     (user-print val)
	     (newline)
	     (next-alternative))
	   ;; ambeval failure
	   (lambda ()
	     (announce-output
	      ";;; There are no more values")
	     (display positions-tried))))


;; ex 4.41
(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (test-dwelling b c f m s)
  (if (and (distinct? (list b c f m s))
	   (not (= b 5))
	   (not (= c 1))
	   (not (= f 5))
	   (not (= f 1))
	   (> m c)
	   (not (= (abs (- s f)) 1))
	   (not (= (abs (- f c)) 1)))
      (let ((result (list (list 'baker b)
			  (list 'cooper c)
			  (list 'fletcher f)
			  (list 'miller m)
			  (list 'smith s))))
	(display result)
	(newline))))
	   
(define (multiple-dwelling-scheme)
  (let ((floors (list 1 2 3 4 5)))
    (map (lambda (baker)
	   (map (lambda (cooper)
		  (map (lambda (fletcher)
			 (map (lambda (miller)
				(map (lambda (smith) 
				       (test-dwelling 
					baker 
					cooper 
					fletcher 
					miller 
					smith))
				     floors))
			      floors))
		       floors))
		floors))
	 floors)
    'ok))

;; Ex 4.45
;; (sentence 
;;  (simple-noun-phrase (article the) (noun professor)) 
;;  (verb-phrase 
;;   (verb-phrase 
;;    (verb lectures) 
;;    (prep-phrase (prep to) 
;; 		(simple-noun-phrase (article the) (noun student)))) 
;;   (prep-phrase (prep in) 
;; 	       (noun-phrase 
;; 		(simple-noun-phrase (article the) (noun class)) 
;; 		(prep-phrase (prep with) 
;; 			     (simple-noun-phrase (article the) (noun cat)))))))
;; ;; Class with the cat

;; (sentence 
;;  (simple-noun-phrase (article the) (noun professor)) 
;;  (verb-phrase 
;;   (verb-phrase 
;;    (verb lectures) 
;;    (prep-phrase (prep to) 
;; 		(noun-phrase 
;; 		 (simple-noun-phrase (article the) (noun student)) 
;; 		 (prep-phrase (prep in) 
;; 			      (simple-noun-phrase (article the) (noun class)))))) 
;;   (prep-phrase (prep with) 
;; 	       (simple-noun-phrase (article the) (noun cat)))))

;; ;; Professor lectures with the cat.  The student is in class. 

;; ;;; Amb-Eval value:
;; ;;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

;; ;;; Amb-Eval value:
;; ;;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))

;; (sentence 
;;  (simple-noun-phrase (article the) (noun cat)) 
;;  (verb-phrase 
;;   (verb eats) 
;;   (prep-phrase (prep in) 
;; 	       (simple-noun-phrase (article the) (noun class)))))
