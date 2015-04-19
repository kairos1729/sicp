(define tolerance 0.00001)

(define (fixed-point f first-guess)
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

(define (cont-frac-r n d k)
  (define (cont-frac-level i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-level (inc i))))))
  (cont-frac-level 1))

(define (cont-frac n d k)
  (define (cont-frac-level-iter i result)
    (if (= i 0)
        result
        (cont-frac-level-iter (dec i) (/ (n i) (+ (d i) result)))))
  (cont-frac-level-iter k 0))

(define (euler-e k)
  (define (euler-d i)
    (let ((x (+ i 1)))
      (if (= (remainder x 3) 0)
          (/ (* x 2) 3)
          1)))
  ( + 2 (cont-frac (lambda (x) 1.0)
             euler-d
             k)))

(define (tan-cf x k)
  (define (n i)
    (if (< i 2)
        x
        (* -1 x x)))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))

