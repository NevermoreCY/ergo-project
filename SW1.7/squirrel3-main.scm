;(include "sw-bridge.scm")
(include "target-bridge.scm")
(include "squirrel3-bat.scm")
(include "squirrel3-procs.scm")

(define (run-to x y)         ; run to (x,y) but east/west on x-axis only
  (:begin
    ;(:for-all i yposition (go-dir 'south))
    (:if (< x xposition) (:for-all i (- xposition x) (go-dir 'west))
        (:for-all i (- x xposition) (go-dir 'east)))
    (:if (< y yposition) (:for-all i (- yposition y) (go-dir 'south))
        (:for-all i (- y yposition) (go-dir 'north)))
    ))
    ;(:for-all i y (go-dir 'north))))

(define (do-at-nest prog)    ; run to (1,1), do prog, then run back
  (:let ((x xposition) (y yposition)) (run-to 0 0) prog (run-to x y)))

(define (stroll-north)       ; walk north, smelling and looking for walls
  (:until (or (eq? seen 'wall) (> yposition (/ ymax 4)))
    (go-dir 'north)
    (check smell) (check look)))

(define (pick-and-drop)    ; pick the 4th and 3rd acorns, drop them at 2nd acorn's location
                           ; then pick 1st acorn and drop at 2nd acron's location
  (:let ((x1 (caar acorns)) (y1 (cadar acorns)) ; first acorn
         (x2 (caadr acorns)) (y2 (cadadr acorns)) ; second acorn
         (x3 (caaddr acorns)) (y3 (car (cdaddr acorns))) ; third acorn
         (x4 (caar (cdddr acorns))) (y4 (cadar (cdddr acorns))) ; forth acorn
                            )

        (:act think)
        (:act pick)
        (run-to x3 y3) (:act pick)
        (run-to x2 y2) (:act drops) (:act drops)
        (run-to x1 y1) (:act pick)
        (run-to x2 y2) (:act drops)
        (run-to x4 y4) (:act resetMem) (go-dir 'north)

    )
  )



(define (main)    ; main function            
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
