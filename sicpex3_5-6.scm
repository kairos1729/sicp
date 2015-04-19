(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else 
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo 
      trials 
      (lambda () 
	(p 
	 (random-in-range x1 x2) 
	 (random-in-range y1 y2))))
     (* (- x2 x1) (- y2 y1))))

(define (make-circle-predicate radius centre-x centre-y)
  (lambda (x y) 
    (<= (+ 
	 (square (- x centre-x)) 
	 (square (- y centre-y))) 
	(square radius))))

(define (circle-area radius centre-x centre-y trials)
  (estimate-integral
   (make-circle-predicate radius centre-x centre-y)
   (- centre-x radius)
   (+ centre-x radius)
   (- centre-y radius)
   (+ centre-y radius)
   trials))

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((state 0))
    (define (generate)
      (set! state (rand-update state))
      state)
    (define (reset x)
      (set! state x)
      x)
    (lambda (m)
      (cond ((eq? m 'generate) (generate))
	    ((eq? m 'reset) reset)
	    (else error "Undefined operation rand" m)))))



