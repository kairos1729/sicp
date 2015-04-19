(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b n)
  (define (make-even x) 
    (if (even? x) x (+ x 1)))
  (integral-even-n f a b (make-even n)))

(define (integral-even-n f a b n)
  (define h 
    (/ (- b a) n))
  (define (coeff k)
    (cond((= k 0) 1)
         ((= k n) 1)
         ((even? k) 2)
         (else 4)))
  (define (y k) 
    (f (+ a (* k h))))
  (define (simpson-term k)
    (* (coeff k) (y k)))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

