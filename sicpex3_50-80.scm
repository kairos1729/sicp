(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (square x)
  (* x x))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-enumerate-interval from to)
  (if (> from to)
      the-empty-stream
      (cons-stream 
       from 
       (stream-enumerate-interval (+ from 1) to))))

(define (stream-for-each proc . argstreams)
  (if (stream-null? (car argstreams))
      'done
      (begin
       (apply proc (map stream-car argstreams))
       (apply stream-for-each
	      (cons proc (map stream-cdr argstreams))))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream)) 
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;;(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;;(display sum)
;;(define y (stream-filter even? seq))
;;(display sum)
;;(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) 
;;			 seq))
;;(display sum)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (take stream n)
  (if (< n 1)
      the-empty-stream
      (cons-stream (stream-car stream)
		   (take (stream-cdr stream) (- n 1)))))

(define (skip stream n)
  (if (< n 1)
      stream
      (skip (stream-cdr stream) (- n 1))))

(define factorials 
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

; ex 3.55
; inefficient if we have memoization:
(define (partial-sumsf s)
  (cons-stream 
   (stream-car s) 
   (add-streams (partial-sumsf s) (stream-cdr s))))

; more efficient when we have memoization:
(define (partial-sums s)
  (define p (cons-stream 
	     (stream-car s) 
	     (add-streams p (stream-cdr s))))
  p)

; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))
(define (scale-stream s n)
  (stream-map (lambda (x) (* x n)) s))

(define hamming 
  (cons-stream 
   1 
   (merge (scale-stream hamming 2)
	  (merge (scale-stream hamming 3)
		 (scale-stream hamming 5)))))

; Inefficient if we have memoization:
(define (fibsf)
  (display-line "fibs")
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr (fibsf))
					 (fibsf)))))
(define ones (cons-stream 1 ones))

(define zeroes (cons-stream 0 zeroes))

; 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; 3.59
(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (stream-fold op a s)
  (if (stream-null? s)
      a
      (stream-fold op (op a (stream-car s)) (stream-cdr s))))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-series s1 (stream-cdr s2))
			    (scale-stream (stream-cdr s1) (stream-car s2)))))

(define sinsq-series (mul-series sine-series sine-series))
(define cossq-series (mul-series cosine-series cosine-series))
(define cossqplussinsq (add-streams sinsq-series cossq-series))

(define (invert-unit-series s)
  (define i (cons-stream 1 
	       (scale-stream (mul-series (stream-cdr s) i) -1)))
i)

(define inverse-cosine-series
  (invert-unit-series cosine-series))

(define (div-series n d)
  (let ((d0 (stream-car d)))
    (if (= 0 d0)
	(error "denominator should have non-zero constant term")
	(scale-stream 
	 (mul-series n
		     (invert-unit-series 
		      (scale-stream d (/ 1 d0))))
	 (/ 1 d0)))))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; 3.64
(define (stream-limit s t)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) t)
	s1
	(stream-limit (stream-cdr s) t))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define eulear-ln2-stream
  (euler-transform ln2-stream))

(define accelerated-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))

; 3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (ss s n)
  (display-stream (take s n)))

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
		(stream-cdr s)))))

; 3.68
(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))

; 3.69
(define (triples-wrong s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave 
    (interleave
     (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
		 (stream-cdr u))
     (stream-map (lambda (x) (list (stream-car s) (stream-car (stream-cdr t)) x))
		 (stream-cdr u)))
    (triples-wrong (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triples-wrong-int (triples-wrong integers integers integers))

(define (pythagorean? t)
  (= (+ (square (car t)) (square (cadr t))) (square (caddr t))))

(define pyth-triples-wrong (stream-filter pythagorean? triples-wrong-int))

; ... had to look up the correct answer :(
(define (triples2 s t u)
  (cons-stream (list 
		(stream-car s)
		(stream-car t) 
		(stream-car u))
	       (interleave
		(stream-map (lambda (x) (cons (stream-car s) x))
			    (stream-cdr (pairs t u)))
		(triples2 (stream-cdr s)
			 (stream-cdr t)
			 (stream-cdr u)))))

(define triples-int2 (triples2 integers integers integers))

(define (phythagorean-numbers)
  (define (square x) (* x x))
  (define numbers (triples2 integers integers integers))
  (stream-filter (lambda (x) 
		   (= (square (caddr x)) 
		      (+ (square (car x)) (square (cadr x)))))
		 numbers))
 
; 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (let ((w1 (weight s1car))
		 (w2 (weight s2car)))
	     ;(display-line s1car)
	     ;(display-line w1)
	     ;(display-line s2car)
	     ;(display-line w2)
	     (cond ((< w1 w2)
		    ;(display-line "<")
		    (cons-stream s1car 
				 (merge-weighted 
				  (stream-cdr s1) s2 weight)))
		   (else
		    ;(display-line ">=")
		    (cons-stream s2car 
				 (merge-weighted 
				  s1 (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

 (define (weight-sum p)
   (+ (car p) (cadr p)))

(define (weight-235 p)
  (let ((i (car p))
	(j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define pairs-by-sum (weighted-pairs integers integers weight-sum))

(define not235 
  (stream-filter (lambda (x) (not (or 
				   (= (remainder x 2) 0)
				   (= (remainder x 3) 0)
				   (= (remainder x 5) 0))))
		 integers))

(define not235pairs-ordered (weighted-pairs not235 not235 weight-235))

; Ex 3.71
(define (sum-cubes-weight p)
  (let ((i (car p))
	(j (cadr p)))
    (+ (* i i i) (* j j j))))

(define sum-cubes-ordered-pairs 
  (weighted-pairs integers integers sum-cubes-weight))

(define ramanujan-numbers
  (stream-filter 
   list?
   (stream-map (lambda (x y)
		 (if (= (sum-cubes-weight x) (sum-cubes-weight y))
		     (list (sum-cubes-weight x) x y)
		     'ignore))
	       sum-cubes-ordered-pairs
	       (skip sum-cubes-ordered-pairs 1))))

; Ex 3.72
(define (sum-squares-weight p)
  (let ((i (car p))
	(j (cadr p)))
    (+ (* i i) (* j j))))

(define sum-squares-ordered-pairs 
  (weighted-pairs integers integers sum-squares-weight))

(define sum-squares-numbers
  (stream-filter 
   list?
   (stream-map (lambda (x y z)
		 (if (= 
		      (sum-squares-weight x) 
		      (sum-squares-weight y) 
		      (sum-squares-weight z))
		     (list (sum-squares-weight x) x y z)
		     'ignore))
	       sum-squares-ordered-pairs
	       (skip sum-squares-ordered-pairs 1)
	       (skip sum-squares-ordered-pairs 2))))

; Could generalise the last two exercises but I've run out of time today...

; Ex 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

(define (RC r c dt)
  (define (resp i v0)
    (let ((sr (scale-stream i r))
	  (sc (scale-stream i (/ 1 c))))
      (let ((integral-sc (integral sc v0 dt)))
	(add-streams integral-sc sr))))
  resp)

(define RC1 (RC 5 1 0.5))

(define ramp-up (RC1 ones 0))

; Ex 3.74
(define (sign-change-detector current last)
  (cond ((and (positive? current) (negative? last)) 1)
	((and (negative? current) (positive? last)) -1)
	(else 0)))

(define (tostream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (tostream (cdr l)))))

(define sense-data (tostream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

; Ex 3.75
(define (make-zero-crossings input-stream last-original-value last-average-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-original-value) 2)))
    (cons-stream (sign-change-detector avpt last-average-value)
		 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream)
				      avpt))))

; ex 3.76
(define (average a b) (/ (+ a b) 2))

(define (smooth s) (stream-map average (stream-cdr s) s))

(define (sign-changes s)
  (stream-map sign-change-detector s (cons-stream 0 s)))

(define (make-zero-crossings2 s)
  (sign-changes (smooth s)))

; ex 3.77
(define (integral2 delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral2 (delay (stream-cdr integrand))
				(+ (* dt (stream-car integrand))
				   initial-value)
				dt)))))

(define (solve f y0 dt)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; ex 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (integral2 (delay ddy) dy0 dt))
  (define (f dyval yval)
    (+ (* dyval a) (* yval b)))
  (define ddy (stream-map f dy y))
  y)

; ex 3.79
(define (solve-2ndf f dt y0 dy0)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (integral2 (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (solve-2ndb a b dt y0 dy0)
  (define (f dyval yval)
    (+ (* dyval a) (* yval b)))
  (solve-2ndf f dt y0 dy0))

; ex 3.80
(define (RLC r l c dt)
  (define (response vc0 il0)
    (define vc (integral2 (delay dvc) vc0 dt))
    (define il (integral2 (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 c)))
    (define dil 
      (stream-map 
       (lambda (ilval vcval) (+ (/ (* -1 r ilval) l) (/ vcval l))) 
       il vc))
    (cons vc il))
  response)

; ex 3.81
(define random-init 0)

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-numbers
  (cons-stream random-init
	       (stream-map rand-update random-numbers)))

(define (random-numbers-from requests)
  (define (next request current)
    (if (eq? request 'g)
	(rand-update current)
	request))
  (define r
    (cons-stream (next (stream-car requests) random-init)
		 (stream-map next (stream-cdr requests) r)))
  r)
 
(define (make-stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (make-stream (cdr l)))))

; ex 3.82
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (randoms-in-range low high)
  (cons-stream (random-in-range low high) (randoms-in-range low high)))

(define (integrals-experiment p x1 x2 y1 y2)
  (stream-map p (randoms-in-range x1 x2) (randoms-in-range y1 y2)))

(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (x) (* x (- x2 x1) (- y2 y1))) 
	      (monte-carlo (integrals-experiment p x1 x2 y1 y2) 0.0 0.0)))

(define (make-circle-predicate radius centre-x centre-y)
  (lambda (x y) 
    (<= (+ 
	 (square (- x centre-x)) 
	 (square (- y centre-y))) 
	(square radius))))

(define (circle-area radius)
  (estimate-integral
   (make-circle-predicate radius radius radius)
   0
   (* 2 radius)
   0
   (* 2 radius)))

(define (sum s t)
  (if (stream-null? s)
      t
      (sum (stream-cdr s) (+ (stream-car s) t))))
