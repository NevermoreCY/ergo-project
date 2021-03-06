;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target shoot game V4
;; author: YL  , YC
;; date: 04 Nov 2020
;;
;; This is an Ergo implementation of a very simple reactive agent in a game
;; setting that performs goal recognition. It involves discrete and
;; continuous distributions.  The observed agent
;; selects a target, aims at it, and shoots.  Initially the target is
;; unknown, but after making a noisy observation of the aim, the system
;; gets some knowledge of the selected target.
;;
;; After the all the observations of aiming are done, (after the action endObsAim!),
;; the system will shield a target that has the highest belief of decisionTarget.
;; Then the agent will shoot at his chosen target. And system will observe the shooting.
;; If the shield target is different than the shooting target, the target is considered damaged.
;; Otherwise, the traget is hit but un damaged.
;;
;; If the target is damgaged, the agent will win the corresponding score.
;; If the target is not damaged or agent is not hitting any target, agent will lose 20 score.
;; Then everything except score will be reset and next round of game will start.
;; The definition of each round is defined as oneRound.
;; The game ends once the agent call halt! action after a round.
;; If the final score is positive, the agent wins. If it's negative, the system wins.
;; And it's a draw if score equal to zero.
;;  Here, ERGO is used in onlineSynchronized mode where:
;;     - actions/beliefs query results generated by the program
;;       are simply printed
;;     - exogenous actions are read from a file targetShootObs1.txt
;;
;; To run:
;; In a terminal enter "racket -l ergoExt -i -f targetShootGameV4.ergo.scm"
;;               and then run the program by entering "(main)"
;;
;; The file targetShootObs1.txt (in the same directory) should contain
;; the following:
;; chooseTarget!
;; setAim!
;; (obsAim! 90)
;; endObsAim!
;; rest!
;; shoot!
;; (obsHit! 2)
;; reset!
;; chooseTarget!
;; setAim!
;; (obsAim! 0)
;; (obsAim! 30)
;; endObsAim!
;; rest!
;; shoot!
;; (obsHit! 0)
;; reset!
;; chooseTarget!
;; setAim!
;; (obsAim! 60)
;; (obsAim! 65)
;; endObsAim!
;; rest!
;; shoot!
;; (obsHit! 1)
;; reset!
;; halt!


;;
;; In the terminal, Ergo displays the updated beliefs, which evolve as expected.
;;
;; After (obsAim! 90), endObsAim! is entered, the system thinks that it is very
;; likely that target 3 is aimed at. And (shield!3) will be performed after wards
;; Then, the system observed a hit at target 3. the target is undamaged
;; since it's been shielded by the system. Thus score is reduced by 20. Then second round starts when agent
;; call chooseTarget! action after reset!. This time the system observes the
;; the agent is aiming at target 0. And it shields target zero and they agent lose 20 score again. So as the
;; third round where agent aims at target 1.
;; 
;; There are several more interesting observation files in the fold, one should take a look along with the updated beliefs. 
;; (The updated beliefs for each observation are also saved as targetShootObs1Run1 )
;;




;; Auxiliary definitions including non-fluents 

(define (remainder n m)
  (- n (* (floor (/ n m)) m)))

(define nTargets 3)
  
(define targetPosition (vector 30.0 60.0 90.0))

(define targetSize (vector 10.0 30.0 20.0))

(define targetValue (vector 30.0 10.0 20.0))

;; Auxiliary definitions 

(define (aimedAtTarget)
  (let loop ((t 0))
    (if (>= t nTargets) #f
	(if (and (>= aim (- (vector-ref targetPosition t)
			    (/ (vector-ref targetSize t) 2)))
		 (<= aim (+ (vector-ref targetPosition t)
			    (/ (vector-ref targetSize t) 2))))
	    t
	    (loop (+ t 1))))))

;; we use an alternative version of non-deterministic iteration
;; defined as follows

(define (:starDFS prog)
  (:choose :nil (:begin prog (:starDFS prog))))

(define (oneRound)
  (:begin
   (:act chooseTarget!)
   (:act setAim!)   ; one can replace this line with (:act nSetAim!) to add noise
   (:starDFS (:act (obsAim! someObservedAim)))
   (:act endObsAim!)
   (:act rest!)
   (:act shoot!)
   (:choose (:for-some t '(0 1 2) (:act (obsHit! t)))
			   (:act obsNoHit!))
   (:act reset!)
   )
  )

;; States and actions

(define-states ((i 100000))
  decisionTarget 'notDecided
  aim 0.0
  someObservedAim 'notSet  
  endObsAim #f          ;; new for targetShootGame
  predicted 'notDecided   ;; new for targetShootGame
  score 0.0            ;; new for targetShootGameV2
  shielded 'notDecided    ;; new for targetShootGameV2
  targetHit (vector #f #f #f)
  ;; Target canbe hit but not damaged since the system can shield the target
  targetDamaged (vector #f #f #f)
  halted #f
  
  exoProg (:begin (:starDFS (oneRound))
		  (:act halt!)
		  )
  )






(define-action reset! #:sequential? #t
  
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'reset! exoProg))) (if res (cadr res) :fail))
  ;;reset everything except score and halted
  decisionTarget 'notDecided
  aim 0.0
  someObservedAim 'notSet  
  endObsAim #f          
  predicted 'notDecided           
  shielded 'notDecided   
  targetHit (vector #f #f #f)
  targetDamaged (vector #f #f #f)
  weight (/ 1 100000)
  
  )

(define-action halt! #:sequential? #t
  halted #t
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'halt! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

;; accurate decision action
(define-action chooseTarget! #:sequential? #t
  decisionTarget (DISCRETE-GEN 0 0.33 1 0.33 2 0.34)
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'chooseTarget! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

;; accurate unobservable actuation action
(define-action setAim! #:sequential? #t
  aim (vector-ref targetPosition decisionTarget)
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'setAim! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

;; noisy unobservable actuation action
(define-action nSetAim! #:sequential? #t
  aim (GAUSSIAN-GEN (vector-ref targetPosition decisionTarget) 5.0)
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'nSetAim! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

;; noisy continuous sensor
(define-action (obsAim! a)  #:sequential? #t
  ;; #:prereq (and (>= a 0.0) (< a 360.0))
  someObservedAim a ;; fluent someObservedAim is set to argument angle a
  ;; ensuring the exoProg can run to match the action
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch
			      #:matchAct (list 'obsAim! a) exoProg)))
	    (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail)
	     0.0
	     (* weight (GAUSSIAN (remainder a 360.0) aim 10.0)))
  )


 ;; new for targetShootGame
(define-action rest! #:sequential? #t
  ;; rest after all observations are done. We need a rest action in this Online-sychronized mode example since
  ;; we need an extra endogenous action to update shileded fluent.
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'rest! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

 ;; new for targetShootGame
(define-action endObsAim! #:sequential? #t
  ;; an action used to indicate that no more obsAim! actions will be performed.
  endObsAim #t  ;; set endObsAim to true and the system won't change shield target after this
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'endObsAim! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

(define-action (shield! t) #:sequential? #t
  ;; shield the predicted target
  ;; since it's online sychronized version, we shield once we find the predicted target
  shielded t
  )

;; accurate unobservable actuation action
(define-action shoot! #:sequential? #t

    
  ;; update target hit
  targetHit (let ((aat (aimedAtTarget)))
	      (if (not aat) targetHit
		  (vector-set targetHit aat #t)))

  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'shoot! exoProg))) (if res (cadr res) :fail))
  weight (if (equal? exoProg :fail) 0.0 weight)
  )

;; accurate discrete sensor
(define-action (obsHit! t) #:sequential? #t

  ;; update target damaged 
  targetDamaged (if (eq? t shielded) targetDamaged
		  (vector-set targetDamaged t #t))

  
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch
			      #:matchAct (list 'obsHit! t) exoProg)))
		(if res (cadr res) :fail))
  ;; update score
  score   (if (= t shielded) (- score 20.0)
		  (+ score (vector-ref targetValue t)) )
  weight (if (and (vector-ref targetHit t)
		  (not (equal? exoProg :fail)))
	     weight
	     0.0)
  )

;; accurate discrete sensor
(define-action obsNoHit! #:sequential? #t

  
  exoProg (let ((res (ergo-do #:mode 'offlineStepMatch #:matchAct 'obsNoHit! exoProg))) (if res (cadr res) :fail))

  ;; score is reduced by 20 if no target is hit.
  score (- score 20.0)
  
  weight (if (and (not (or-map (lambda (i) (vector-ref targetHit i))
			       (iota nTargets)))
		  (not (equal? exoProg :fail)))
	     weight
	     0.0
	     )
  )

(define (displayBeliefs)
  (printf  "(beliefl (eq? decisionTarget notDecided)) returns ~s~n" (belief (eq? decisionTarget 'notDecided)))
  (printf  "(belief (eq? decisionTarget 0)) returns ~s~n" (belief (eq? decisionTarget 0)))
  (printf  "(belief (eq? decisionTarget 1)) returns ~s~n" (belief (eq? decisionTarget 1)))
  (printf  "(belief (eq? decisionTarget 2)) returns ~s~n" (belief (eq? decisionTarget 2)))

  (printf  "(belief (aiming at target 0)) returns ~s~n"
	   (belief (and (>= aim (- (vector-ref targetPosition 0)
			    (/ (vector-ref targetSize 0) 2)))
		 (<= aim (+ (vector-ref targetPosition 0)
			    (/ (vector-ref targetSize 0) 2)))))
	   )

  (printf  "(belief (aiming at target 1)) returns ~s~n"
	   (belief (and (>= aim (- (vector-ref targetPosition 1)
			    (/ (vector-ref targetSize 1) 2)))
		 (<= aim (+ (vector-ref targetPosition 1)
			    (/ (vector-ref targetSize 1) 2)))))
	   )
  (printf  "(belief (aiming at target 2)) returns ~s~n"
	   (belief (and (>= aim (- (vector-ref targetPosition 2)
			    (/ (vector-ref targetSize 2) 2)))
		 (<= aim (+ (vector-ref targetPosition 2)
			    (/ (vector-ref targetSize 2) 2)))))
	   )
  
  (printf  "(belief (eq? endObsAim #f)) returns ~s~n" (belief (eq? endObsAim #f)))
  (printf  "(belief (eq? endObsAim #t)) returns ~s~n" (belief (eq? endObsAim #t)))
  
  (printf  "(belief (eq? shielded notDecided)) returns ~s~n" (belief (eq? shielded 'notDecided)))
  (printf  "(belief (eq? shielded 0)) returns ~s~n" (belief (eq? shielded 0)))
  (printf  "(belief (eq? shielded 1)) returns ~s~n" (belief (eq? shielded 1)))
  (printf  "(belief (eq? shielded 2)) returns ~s~n" (belief (eq? shielded 2)))
  (printf  "(belief (eq? shielded none)) returns ~s~n" (belief (eq? shielded 'none)))

  (printf  "(belief (vector-ref targetHit 0)) returns ~s~n"
	   (belief (vector-ref targetHit 0))
	   )

  (printf  "(belief (vector-ref targetHit 1)) returns ~s~n"
	   (belief (vector-ref targetHit 1))
	   )

  (printf  "(belief (vector-ref targetHit 2)) returns ~s~n"
	   (belief (vector-ref targetHit 2))
	   )
  (printf  "(belief (vector-ref targetDamaged 0)) returns ~s~n"
	   (belief (vector-ref targetDamaged 0))
	   )
  (printf  "(belief (vector-ref targetDamaged 1)) returns ~s~n"
	   (belief (vector-ref targetDamaged 1))
	   )
  (printf  "(belief (vector-ref targetDamaged 2)) returns ~s~n"
	   (belief (vector-ref targetDamaged 2))
	   )
  
  (printf  "mean of score ~s~n" (sample-mean score))
  (printf  "variance of score ~s~n" (sample-variance score))

  
  )



(define buStartTime 0)

(define observeUpdtLoop
  (:begin
   (:>>  (let ((start  (current-milliseconds)))
		  (displayBeliefs)
		  (printf "Elapsed time for belief queries and display ~a ms\n"
			  (- (current-milliseconds) start))))
   (:while (< (belief (eq? halted #t)) 0.9)
	   (:>> (set! buStartTime (current-milliseconds)))
           ;; if belief of endObsAim equal to false
	   (:if (> (belief (eq? endObsAim #f)) 0.9) (:begin (:>> (display "test #t\n")) (:wait))
                ;; if the shield target is notDecided
                (if (> (belief (eq? shielded 'notDecided)) 0.9) 

                    ;; we shield the target that has the highest belief of decisionTarget
                    (if (>= (belief (eq? decisionTarget 0)) (belief (eq? decisionTarget 1)) )
                        ;; case when belif of target 0 >= belif of target 1
                        (if (>= (belief (eq? decisionTarget 0)) (belief (eq? decisionTarget 2))) (:act (shield! 0)) (:act (shield! 2)))
                        ;; case when belif of target 1 >= belif of target 0
                        (if (>= (belief (eq? decisionTarget 1)) (belief (eq? decisionTarget 2))) (:act (shield! 1)) (:act (shield! 2)))
                        )
                    ;; if the shield target is chosen after end of obseration, we pass.
                    ;(:test #t)
                    (:wait)
                    )
               )
	   (:>> (printf "Elapsed time for belief update ~a ms\n"
			  (- (current-milliseconds) buStartTime)))
	   (:>>  (let ((start  (current-milliseconds)))
		  (displayBeliefs)
		  (printf "Elapsed time for belief queries and display ~a ms\n"
			  (- (current-milliseconds) start))))

   )
   ;; After the while loop check whos the winner:
   (:when (> (belief (< score 0)) 0.9) (:>> (display "System wins.\n")) )
   (:when (> (belief (> score 0)) 0.9) (:>> (display "Agent wins.\n"))  )
   (:when (> (belief (= score 0)) 0.9) (:>> (display "Draw.\n"))  )
   
  )
)

(define-interface 'out write-endogenous)

;(define-interface 'in
;  (let ((iport (open-input-file "targetShootObs1.txt")))  ; change the input file name here
;    (displayln "Opening file targetShootObs.txt to receive exogenous actions!")
;    (lambda () (let ((exog (read iport)))
;		 exog))))

(define-interface 'in
  (let ((ports (open-tcp-server 8234)))
    (displayln "Ready to receive exogenous actions!" (cadr ports))
    (lambda () (read (car ports)) )))

(define (main) (let ((runStartTime (current-milliseconds)))
		     (ergo-do #:mode 'onlineSynchronized observeUpdtLoop)
		     (printf "Total time for run ~a ms\n"
			  (- (current-milliseconds) runStartTime))
		 ))
