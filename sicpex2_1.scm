(define (numer r)
  (car r))

(define (denom r)
  (cdr r))

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (< (* n d) 0) -1 1)))
    (cons (* sign (abs(/ n g))) (abs(/ d g)))))

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r)))