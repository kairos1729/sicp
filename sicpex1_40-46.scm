(define (iterative-improve close-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  try)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ((iterative-improve
   (lambda (guess next) (< (abs (- guess next)) tolerance))
   f) 
  first-guess))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess next) (< (abs (- (square next) x)) tolerance))
    (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display count)
    (display " ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (inc count)))))
  (try first-guess 1))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (square x)
  (* x x))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter count result)
    (if (< count n)
        (repeated-iter (inc count) (compose f result))
        result))
  (repeated-iter 0 (lambda (x) x)))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))(f x)(f (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (exp b n)
  (define (exp_iter a b n)
    (cond ((= n 0) a)
          ((odd? n) (exp_iter (* a b) b (- n 1)))
          (else (exp_iter a (* b b) (/ n 2)))))
  (exp_iter 1 b n))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root-q x n n-damp)
  (define (f y)
    (/ x (exp y (- n 1))))
  (fixed-point ((repeated average-damp n-damp) f) 1))

(define (nth-root x n)
  (define n-damp (truncate (/ (log n) (log 2))))
  (display "*** n-damp: ")
  (display n-damp)
  (newline)
  (nth-root-q x n n-damp))

