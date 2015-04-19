(define (exp_iter a b n)
  (cond ((= n 0) a)
        ((odd? n) (exp_iter (* a b) b (- n 1)))
        (else (exp_iter a (* b b) (/ n 2)))))

(define (exp b n)
  (exp_iter 1 b n))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (mul-iter x a b)
  (cond ((= b 0) x)
        ((odd? b) (mul-iter (+ x a) a (- b 1)))
        (else (mul-iter x (double a) (halve b)))))

(define (mul a b)
  (mul-iter 0 a b))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (* q (+ (* 2 p) q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fibs n)
  (define (fibs-iter n l)
    (if (= n 0) 
        l
        (fibs-iter (- n 1) (cons (fib n) l))))
  (fibs-iter n nil))

