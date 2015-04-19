(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect 
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect 
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect 
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))

(define (display-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ", ")
  (display (ycor-vect v))
  (display ")"))

(define (make-frame1 o e1 e2)
  (list o e1 e2))

(define (origin1 frame)
  (car frame))

(define (e11 frame)
  (cadr frame))

(define (e21 frame)
  (caddr frame))

(define (make-frame o e1 e2)
  (cons o (cons e1 e2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 11 14))
(define v3 (make-vect 4 8))

(define example-frame (make-frame v1 v2 v3))
(define example-frame1 (make-frame1 v1 v2 v3))

(define (make-segment s e)
  (cons s e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define (draw-line-vect v1 v2)
  ((draw-line vp) 
   (vector-to-posn v1) 
   (vector-to-posn v2)))

(define (vector-to-posn v)
  (make-posn (xcor-vect v) (ycor-vect v)))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (draw-line-vect         
        ((frame-coord-map frame) (start-segment segment))         
        ((frame-coord-map frame) (end-segment segment))))      
      segment-list)))

(define outline-segments 
  (list
   (make-segment (make-vect 0 0) (make-vect 0 1))
   (make-segment (make-vect 0 0) (make-vect 1 0))
   (make-segment (make-vect 0 1) (make-vect 1 1))
   (make-segment (make-vect 1 0) (make-vect 1 1))))

(define outline-segments-painter
  (segments->painter outline-segments))

(define my-square-frame
  (make-frame 
   (make-vect 10 15)
   (make-vect 100 0)
   (make-vect 0 100)))

(define my-big-square-frame
  (make-frame 
   (make-vect 0 0)
   (make-vect 500 0)
   (make-vect 0 500)))

(define my-rhombus-frame
  (make-frame 
   (make-vect 150 150)
   (make-vect 100 10)
   (make-vect 10 100)))

(define x-segments 
  (list
   (make-segment (make-vect 0 0) (make-vect 1 1))
   (make-segment (make-vect 1 0) (make-vect 0 1))))

(define x-segments-painter
  (segments->painter x-segments))

(define diamond-segments 
  (list
   (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
   (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
   (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
   (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

(define diamond-segments-painter
  (segments->painter diamond-segments))

(define wave-segments 
  (list
   (make-segment (make-vect 0 0.8) (make-vect 0.2 0.6))
   (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.65))
   (make-segment (make-vect 0.3 0.65) (make-vect 0.35 0.65))
   (make-segment (make-vect 0.35 0.65) (make-vect 0.3 0.8))
   (make-segment (make-vect 0.3 0.8) (make-vect 0.35 1))
   (make-segment (make-vect 0.65 1) (make-vect 0.7 0.8))
   (make-segment (make-vect 0.7 0.8) (make-vect 0.65 0.65))
   (make-segment (make-vect 0.65 0.65) (make-vect 0.75 0.65))
   (make-segment (make-vect 0.75 0.65) (make-vect 1 0.4))
   (make-segment (make-vect 1 0.2) (make-vect 0.65 0.45))
   (make-segment (make-vect 0.65 0.45) (make-vect 0.8 0))
   (make-segment (make-vect 0.65 0) (make-vect 0.5 0.4))
   (make-segment (make-vect 0.5 0.4) (make-vect 0.35 0))
   (make-segment (make-vect 0.2 0) (make-vect 0.325 0.5))
   (make-segment (make-vect 0.325 0.5) (make-vect 0.3 0.55))
   (make-segment (make-vect 0.3 0.55) (make-vect 0.2 0.4))
   (make-segment (make-vect 0.4 0.7) (make-vect 0.45 0.65))
   (make-segment (make-vect 0.45 0.65) (make-vect 0.55 0.65))
   (make-segment (make-vect 0.55 0.65) (make-vect 0.6 0.7))
   (make-segment (make-vect 0.2 0.4) (make-vect 0 0.6))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define wave-up
  (segments->painter wave-segments))

(define wave (flip-vert wave-up))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (shift-frame frame shift-vect)
  (make-frame (add-vect shift-vect (origin-frame frame))
              (edge1-frame frame)
              (edge2-frame frame)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-above
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 0.5))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

(define (below painter1 painter2)
  (rotate270 
   (beside 
    (rotate180 (rotate270 painter2)) 
    (rotate180 (rotate270 painter1)))))

(define (split main-op split-op)
  (define (new-painter painter n)
    (if (= n 0)
        painter
        (let ((smaller (new-painter painter (- n 1))))
          (main-op painter (split-op smaller smaller)))))
  new-painter)

(define up-split (split below beside))

(define right-split (split beside below))

;(define (corner-split painter n)
;   (if (= n 0)
;      painter
;       (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;         (let ((top-left (beside up up))
;               (bottom-right (below right right))
;               (corner (corner-split painter (- n 1))))
;           (beside (below painter top-left)
;                   (below bottom-right corner))))))

(define (corner-split painter n)
   (if (= n 0)
      painter
       (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
         (let ((top-left up)
               (bottom-right right)
               (corner (corner-split painter (- n 1))))
           (beside (below painter top-left)
                   (below bottom-right corner))))))

;(define (square-limit painter n)
;   (let ((quarter (corner-split painter n)))
;     (let ((half (beside (flip-horiz quarter) quarter)))
;       (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;(define (identity painter)
;  painter)

(define (square-limit painter n)
  (let ((combine4 (square-of-four 
                   flip-horiz 
                   identity 
                   rotate180 
                   flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))



