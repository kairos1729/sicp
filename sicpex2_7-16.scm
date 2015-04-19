(define (make-interval a b) (cons (min a b) (max a b)))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
  
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
  
(define (mul-interval-slow x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (sign x)
  (if (< x 0) -1 +1))

(define (neg? x)
  (< x 0))

(define (pos? x)
  (not (neg? x)))

(define (mul-interval x y)
  (let ((l1 (lower-bound x))
        (u1 (upper-bound x))
        (l2 (lower-bound y))
        (u2 (upper-bound y)))
    (cond ((and (pos? l1) (pos? u1) (pos? l2) (pos? u2)) 
           (make-interval (* l1 l2) (* u1 u2)))
          ((and (pos? l1) (pos? u1) (neg? l2) (pos? u2)) 
           (make-interval (* u1 l2) (* u1 u2)))
          ((and (pos? l1) (pos? u1) (neg? l2) (neg? u2)) 
           (make-interval (* u1 l2) (* l1 u2)))
          ((and (neg? l1) (pos? u1) (pos? l2) (pos? u2)) 
           (make-interval (* l1 u2) (* u1 u2)))
          ((and (neg? l1) (pos? u1) (neg? l2) (neg? u2)) 
           (make-interval (* u1 l2) (* l1 l2)))
          ((and (neg? l1) (neg? u1) (pos? l2) (pos? u2)) 
           (make-interval (* l1 u2) (* u1 l2)))
          ((and (neg? l1) (neg? u1) (neg? l2) (pos? u2)) 
           (make-interval (* l1 u2) (* l1 l2)))
          ((and (neg? l1) (neg? u1) (neg? l2) (neg? u2)) 
           (make-interval (* u1 u2) (* l1 l2)))
          (else (make-interval 
                 (min (* l1 u2) (* u1 l2)) 
                 (max (* l1 l2) (* u1 u2)))))))

(define (spans-zero? interval)
  (not (= 
         (sign (lower-bound interval)) 
         (sign (upper-bound interval)))))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "divisor spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y)) 
                                   (/ 1.0 (lower-bound y))))))

(define ipp1 (make-interval 1 3))
(define inp1 (make-interval -1 3))
(define inn1 (make-interval -1 -3))

(define ipp2 (make-interval 4 8))
(define inp2 (make-interval -4 8))
(define inn2 (make-interval -4 -8))

(define (compare-mul i1 i2)
  (display "i1: ")
  (display i1)
  (display " i2: ")
  (display i2)
  (display " slow: ")
  (display (mul-interval-slow i1 i2))
  (display " fast: ")
  (display (mul-interval i1 i2))
  (newline)
  )

(define i1s (list ipp1 inp1 inn1))
(define i2s (list ipp2 inp2 inn2))

(define (compare-muls-for i1list i2list)
  (define (twos-iter one twos)
    (cond ((not (null? twos))
        (compare-mul one (car twos))
        (twos-iter one (cdr twos)))))
  (cond ((not (null? i1list))
      (twos-iter (car i1list) i2list)
      (compare-muls-for (cdr i1list) i2list))))


  
  
  