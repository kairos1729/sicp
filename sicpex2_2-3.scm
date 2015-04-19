(define (average a b)
  (/ (+ a b) 2))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment-xy start-x start-y end-x end-y)
  (make-segment 
   (make-point start-x start-y) 
   (make-point end-x end-y)))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let((startp (start-segment s))
       (endp (end-segment s)))
    (define (segment-average coord-picker)
      (average (coord-picker startp) (coord-picker endp)))
    (let((x (segment-average x-point))
         (y (segment-average y-point)))
      (make-point x y))))

;(define (make-rect height width centre rotation)
;  (cons height (cons width (cons centre rotation))))
;
;(define (rect-height r)
;  (car r))
;
;(define (rect-width r)
;  (car (cdr r)))

(define (make-rect p1 p2 p3)
  (cons p1 (cons p2 p3)))

(define (rect-height r)
  ((car r))

(define (rect-width r)
  (car (cdr r)))

(define (rect-perimeter r)
  (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))
