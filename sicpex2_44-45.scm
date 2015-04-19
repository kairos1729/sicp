(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split main-op split-op)
  (define (new-painter painter n)
    (if (= n 0)
        painter
        (let ((smaller (new-painter painter (- n 1))))
          (main-op painter (split-op smaller smaller)))))
  new-painter)

(define up-split (split below beside))

(define right-split (split beside below))

(define (corner-split painter n)
   (if (= n 0)
      painter
       (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
         (let ((top-left (beside up up))
               (bottom-right (below right right))
               (corner (corner-split painter (- n 1))))
           (beside (below painter top-left)
                   (below bottom-right corner))))))

(define (square-limit painter n)
   (let ((quarter (corner-split painter n)))
     (let ((half (beside (flip-horiz quarter) quarter)))
       (below (flip-vert half) half))))

