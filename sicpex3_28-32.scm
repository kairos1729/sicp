(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front) 
      (error-if-empty "FRONT")
      (car front-ptr))
    (define (insert! item)
      (let ((new-pair (cons item '()))
	    (old-rear-ptr rear-ptr))
	(set! rear-ptr new-pair)
	(if (null? front-ptr)
	    (set! front-ptr new-pair)
	    (set-cdr! old-rear-ptr new-pair))))
    (define (delete!)
      (error-if-empty "DELETE")
      (set! front-ptr (cdr front-ptr)))
    (define (display-queue) (display front-ptr))
    (define (error-if-empty name)
      (if (empty?)
	  (error name " called with an empty queue" )
	  true))    
    (define (dispatch m)
      (cond ((eq? m 'empty) empty?)
	    ((eq? m 'front) front)
	    ((eq? m 'insert) insert!)
	    ((eq? m 'delete) delete!)
	    ((eq? m 'display) display-queue)
	    (else (error "Unknown queue operation " m))))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty)))

(define (front-queue queue)
  ((queue 'front)))

(define (insert-queue! queue item)
  ((queue 'insert) item)
  queue)

(define (delete-queue! queue)
  ((queue 'delete))
  queue)

(define (display-queue queue)
  ((queue 'display)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay 
		   (lambda () 
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "logical-not: Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value 
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (= s1 0) (= s2 1)) 0)
	((and (= s1 1) (= s2 0)) 0)
	((and (= s1 1) (= s2 1)) 1)
	(else (error "logical-and: Invalid signals" s1 s2))))

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value 
	   (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (= s1 0) (= s2 1)) 1)
	((and (= s1 1) (= s2 0)) 1)
	((and (= s1 1) (= s2 1)) 1)
	(else (error "logical-or: Invalid signals" s1 s2))))

(define (or-gate-compound a b output)
  (let ((na (make-wire))
	(nb (make-wire))
	(naanb (make-wire)))
    (inverter a na)
    (inverter b nb)
    (and-gate na nb naanb)
    (inverter naanb output)
    'ok
))

(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; n increases towards bit 0
(define (ripple-carry-adder a b s c)
  (define (insert-full-adder a b s c-out)
    (if (null? a)
	'ok
	(let ((c-in (make-wire))) ; signal is 0 by default
	  (full-adder (car a) (car b) c-in (car s) c-out)
	  (insert-full-adder (cdr a) (cdr b) (cdr s) c-in))))
  (insert-full-adder a b s c))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time) (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments) (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments! 
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda ia empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display " New-value = ")
		 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; test our ripple-carry adder for three bits
(define (ripple-carry-add-test a b)
  (define (wires-for bits)
					;(display "wires-for ")
					;(display bits)
					;(newline)
    (map 
     (lambda (bit) 
       (let ((w (make-wire)))
					;(display "set-signal! wire ")
					;(display w)
					;(display " bit ")
					;(display bit)
					;(newline)
	 (set-signal! w bit)
	 w))
     bits))
  (define (sum-wires-for bits)
					;(display "wires-for ")
					;(display bits)
					;(newline)
    (map (lambda (x) (make-wire)) bits))
  (define (probe-wires prefix wires)
    (define (probe-wires-iter wires n)
      (if (not (null? wires))
	  (begin
	    (probe (list prefix n) (car wires))
	    (probe-wires-iter (cdr wires) (+ n 1)))))
    (probe-wires-iter wires 1))
  
  (let ((a-wires (wires-for a))
	(b-wires (wires-for b))
	(sum-wires (sum-wires-for a))
	(carry-wire (make-wire)))
    (probe-wires 'sum sum-wires)
    (probe 'carry carry-wire)
    (ripple-carry-adder a-wires b-wires sum-wires carry-wire)
    (cons carry-wire sum-wires)))

(define (wire-values wires)
  (map (lambda (x) (get-signal x)) wires))

(define (run-adder a b)
  (set! the-agenda (make-agenda))
  (let ((s (ripple-carry-add-test a b)))
    (propagate)
    (newline)
    (newline)
    (display "Result: ")
    (display (wire-values s))))

