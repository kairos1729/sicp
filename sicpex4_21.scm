(define (lambdafac n)
  ((lambda (n)
     ((lambda (fact)
	(fact fact n))
      (lambda (ft k)
	(if (= k 1)
	    1
	    (* k (ft ft (- k 1)))))))
   n))

(define (lambdafib n)
  ((lambda (n)
     ((lambda (fib)
	(fib fib n))
      (lambda (fb k)
	(cond ((= k 0) 0)
	      ((= k 1) 1)
	      (else (+ (fb fb (- k 1)) (fb fb (- k 2))))))))
   n))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= 0 n) 'even (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= 0 n) 'odd (ev? ev? od? (- n 1))))))

   