;; Fluents
(define-fluents  
  p  #f
  )

;; Actions
(define-action a)

(define-action b
p #t)

(define-action c)

(define-action d
	       #:prereq	p
)

; to display the state
(define (show-state)
   (printf "The value of p is ~a.\n" p))
