(define-fluents
  seen 'nothing smelled 0 energy 100 carrying 0 stashed 0
  xposition 0 yposition 0 direction 'north)

;;  motion actions
(define-action quit)
(define-action forward ;moving forward
   xposition (+ xposition (case direction ((east) 1) ((west) -1) (else 0)))
   yposition (+ yposition (case direction ((north) 1) ((south) -1) (else 0)))
   smelled   0
   seen      'nothing)
(define-action left  ; turn left
   direction (cadr (assq direction dirL))
   seen      'nothing)
(define-action right  ;turn right 
   direction (cadr (assq direction dirR))
   seen      'nothing)

;;  acorn actions
(define-action eat  ; eat an acorn   
   carrying  (- carrying 1))
(define-action drops  ; drop an acorn 
   carrying  (- carrying 1)
   stashed   (+ stashed 1))
(define-action pick  ; pick an acorn
   carrying  (+ carrying 1)
   stashed   (if (and (= xposition 1) (= yposition 1))
                (- stashed 1)  ; picking from stash
                stashed))      ; picking from elsewhere


;; letting observeagent know a plan is chosen
(define-action choosePlan!)

;;  sensing actions 
(define-action feel)
(define-action look)
(define-action smell)
(define-action think)
;;  exogenous actions receiving from server
(define-action (set-energy! x) energy x)
(define-action (set-view! x)  seen x)
(define-action (set-aroma! x) smelled (car x))


