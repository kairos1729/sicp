(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (prime? n)
  (= (smallest-divisor2 n) n))

(define (identity x) 
  x)

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term1 x)
    (/ (* 2 x) (+ (* 2 x) 1)))
  (define (term2 x)
    (/ (* 2 (+ x 1)) (+ (* 2 x) 1)))
  (* 4.0 (product term1 1 inc n) (product term2 1 inc n)))

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-r term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-r term a next b)
  (accumulate-r + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-r term a next b)
  (accumulate-r * 1 term a next b))

(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (if (filter? a) (combiner result (term a)) result))))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-relative-primes n)
  (define (relative-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate relative-prime? * 1 identity 1 inc (- n 1)))
