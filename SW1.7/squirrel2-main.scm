;(include "sw-bridge.scm")
(include "target-bridge.scm")
(include "squirrel2-bat.scm")
(include "squirrel2-procs.scm")

(define (run-to x y)         ; run to (x,y) but east/west on x-axis only
  (:begin
    (:for-all i yposition (go-dir 'south))
    (:if (< x xposition) (:for-all i (- xposition x) (go-dir 'west))
        (:for-all i (- x xposition) (go-dir 'east)))
    (:for-all i y (go-dir 'north))))

(define (do-at-nest prog)    ; run to (1,1), do prog, then run back
  (:let ((x xposition) (y yposition)) (run-to 0 0) prog (run-to x y)))

(define (stroll-north)       ; walk north, smelling and looking for walls
  (:until (or (eq? seen 'wall) (> yposition (/ ymax 4)))
    (go-dir 'north)
    (check smell) (check look)))

(define (main)      ; main function            
  (ergo-do #:mode 'online
    (:begin
     (:act choosePlan!)
     (:act think)           
    (:monitor (acorns-present) ; modefied so feel is removed
      (:while #t             ; systematic search of grid
         (stroll-north)             ; walk north as far as possible
         (run-to xposition 0)       ; run back to x-axis
         (go-dir 'east)             ; take one step east
         (check smell)))
    )

    ))
