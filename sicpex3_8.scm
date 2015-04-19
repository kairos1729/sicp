(define f
  (let ((state 1))
    (lambda (x)
      (display x)
      (newline)
      (if (= state x)
	  1
	  (begin (set! state x)
		 0)))))


