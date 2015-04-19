(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square-check (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square-check n m)
  (if (non-trivial-sqrt-1-mod-n? n m) 0 (square n)))

(define (non-trivial-sqrt-1-mod-n? a n)
  (cond ((= a 1) false)
        ((= a (- n 1)) false)
        (else (= 1 (remainder (square a) n)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 2)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (cond ((divides? 2 from)
          (search-for-primes (+ from 1) to))
         ((< from to)
         (timed-prime-test from)
         (search-for-primes (+ from 2) to))
        (else 
         (newline)
         (display "done"))))

(define (all-congruent n)
  (all-congruent-iter (- n 1) n))

(define (all-congruent-iter a n)
  (cond ((< a 1) true)
        ((congruent? a n) (all-congruent-iter (- a 1) n))
        (else false)))

(define (congruent? a n)
  (= (expmod a n n) a))

(define (carmichael? n)
  (and (all-congruent n) (not (prime? n))))

(define (find-carmichaels n)
  (cond ((> n 0) 
         (cond ((carmichael? n)
               (display n)
               (newline)))
         (find-carmichaels (- n 1)))
        (else (display "done"))))
  






  
  