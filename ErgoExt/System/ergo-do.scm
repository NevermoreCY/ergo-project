;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is loaded on demand with (ergo-do ...)
;;; The top level program is ergo-do-fn 
;;; The optional parameter #:mode is
;;;      'offline  -- offline, prompt for keep or discard
;;;      'first    -- offline, return first legal execution (default)
;;;      'count    -- offline, return number of legal executions
;;;      'online   -- online
;;;      'offlineStepAsk -- offline, prompt after each action
;;;      'collecting  -- offline, return list of all legal executions
;;;      'offlineStep -- offline, print every first action and remaining program
;;;      'offlineStepColl -- offline, returns list of all possible first
;;;                          actions and remaining programs
;;;      'offlineStepMatch -- offline, returns a pair of action and first
;;;                           remaining program for the given action in 
;;;                           optional parameter :matchAct or #f if none
;;;      'onlineSynchronized -- online, but with only one exogenous action
;;;                             at each transition

;; the function used by :act and :test, set in with-online-xeq with-offline-xeq
(define ergo-trans (void))

;;  To block and unblock waiting for an exog act via :wait
(define exog-wait-chan (make-channel)) 
(define (ergo-wait-for-exog . args)
  (if (null? args) (channel-put exog-wait-chan #t)
      (sync/timeout (car args) (channel-put-evt exog-wait-chan #t))))
(define (unwait-for-exog) (channel-try-get exog-wait-chan))

(define configList8247 '()) ; added by Yves

;; the top-level function  ...  body at end
(define (ergo-do-fn pgm #:mode [mode 'first] #:matchAct [matchAct #f])  ; closing paren on last line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transfer of actions to and from the outside world via threads

(define mon-endo-chans '())                    ; for syncing with monitors
(define mon-exog-chans '())                    ; for syncing with monitors

;; print error message and abort execution
(define (ergo-abort x)
  (eprintf "~a\nAborting after online error..." (exn-message x))
  (sleep 1) (exit 0))
  
;; return a function that behaves like fn, but that traps errors and aborts
(define (trap-errors msg fn)
  (lambda args
    (call-with-exception-handler
     (lambda (x) (eprintf "~a\n" msg) (ergo-abort x))
     (lambda () (apply fn args)))))

;; handle one exog interface:  call given reader and put results on chan
(define (run-exog-interface ifc chan)
  (let ((reader (trap-errors "Error getting exogenous action" (ifc))))
    (let loop ()
      (let ((a (reader)))
        (when a (unwait-for-exog) (channel-put chan a) (sleep)))
      (loop))))

;; handle one endo interface:  get act from chan and call given writer on it
(define (run-endo-interface ifc chan)
  (let ((writer (trap-errors "Error sending endogenous action" (ifc))))
    (let loop ()
      (let ((a (channel-get chan))) (when a (writer a) (sleep .1)))
      (loop))))
          
;;  Start threads for each declared interfaces
(define (start-ergo-monitors)
  (eprintf "Starting monitors for ~a exogenous and ~a endogenous interfaces.\n"
           (length ifc-exog-acts) (length ifc-endo-acts))
  (set! mon-exog-chans (map (lambda (ifc) (make-channel)) ifc-exog-acts))
  (set! mon-endo-chans (map (lambda (ifc) (make-channel)) ifc-endo-acts))
  (for ((ifc ifc-exog-acts) (chan mon-exog-chans))
    (thread (lambda () (run-exog-interface ifc chan))))
  (for ((ifc ifc-endo-acts) (chan mon-endo-chans))
    (thread (lambda () (run-endo-interface ifc chan)))))

;;  Stop all the threads that handle the interfaces
(define (stop-ergo-monitors)
  (for-each (lambda (chan) (channel-put chan #f)) mon-endo-chans)  ; pause
  (eprintf "Stopping all interface monitors.\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transitions: online and offline

;; make the transition but keep the fail, allowing for backup!
(define (ergo-trans-offline a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
          (begin (change-state a) (succ (cons a old-hist) fail #f)))))

;; For online, handle all exog actions, handle given action, and then
;; make the state transition and commit by using online-fail as continuation
(define (ergo-trans-online a old-hist fail succ)
    (let loop ((exog (ormap channel-try-get mon-exog-chans)))
      (when exog
            (change-state exog)
            (loop (ormap channel-try-get mon-exog-chans))))
    (if a 
        (if (not (legal-action? a)) (fail)
            (begin (for ((chan mon-endo-chans)) (channel-put chan a))
                   (sleep) (change-state a)
                   (succ old-hist online-fail #f)))
        (succ old-hist online-fail #f)))

;; For online, handle A SINGLE EXOG ACTION, handle given action, and then
;; make the state transition and commit by using online-fail as continuation
(define (ergo-trans-online-synchronized a old-hist fail succ)
    (let ((exog (ormap channel-try-get mon-exog-chans)))
      (when exog 
	    (printf "<<< Exogenous act: ~a\n" exog)
            (change-state exog)))
    (if a 
        (if (not (legal-action? a)) (fail)
            (begin (for ((chan mon-endo-chans)) (channel-put chan a))
                   (sleep) (change-state a)
                   (succ old-hist online-fail #f)))
        (succ old-hist online-fail #f)))

;; propose action and if user agrees make the transition but keep the fail,
;; allowing for backup!
(define (ergo-trans-offline-step-ask a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
	  (begin (printf "\nin history ~a can do action ~a\n" (reverse old-hist) a)
		 (if (eq? (ergo-read #"proceed with this action? (y or n) ") 'y)
		     (begin (change-state a) (succ (cons a old-hist) fail #f))
		     (fail))))))

;; print action and remaining program and backtrack
(define (ergo-trans-offline-step a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
	  (begin (printf "\ncan do action ~a with remaining program ~a\n" a succ)
		 (fail)))))

;; save action and remaining program and backtrack
(define (ergo-trans-offline-collect-step a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
	  (begin (set! configList8247 (cons (list a succ) configList8247))
		 (fail)))))

;; if action matches, return remaining prog else backtrack
(define (ergo-trans-offline-step-match a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
	  (if (equal? a matchAct)
	      (list a succ)
	      (fail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; What to do as programs start or end

;; the normal last resort failure: just return #f
(define (offline-fail) #f)

;; the last resort offline final: return history of actions (reversed)
(define (just-hist hist fail) (reverse hist))

;; a last resort offline final: return 'halt
(define (just-succeed hist fail) 'halt)

;; the normal online failure: return #f
(define (online-fail) (eprintf "WARNING: Online program failure\n") #f)

;; the last resort online final: just end 
(define (online-done hist fail) (eprintf "Online program complete\n"))

;; the normal offline final: maybe return list of actions (reversed)
(define (try-final hist fail)
  (let ((rhist (reverse hist)))
    (printf "\n~a\n" rhist)
    (if (eq? (ergo-read #"OK? (y or n) ") 'y) rhist (fail))))

;; offline final where we call failure continuation to try other paths (new YL)
(define (fail-final hist fail) (fail))
  
;; all interpreters start here
(define (common-do pgm fail final)
  (define (succ h f c) (if c (c h f succ) (final h f)))
  (pgm '() fail succ))

;; a variant of common-do for counting executions
(define (counting-do pgm)
  (let ((m 0))
    (common-do pgm offline-fail (lambda (h f) (set! m (+ 1 m)) (f)))
    m))

;; a variant of common-do for collecting all executions -- new YL
(define (collecting-do pgm)
  (let ((l '()))
    (common-do pgm offline-fail (lambda (h f) (set! l (cons (reverse h) l)) (f)))
    l))

;; a variant of common-do for collecting all next actions and remaining programs -- new YL
(define (collecting-step-do pgm)
  (begin
    (set! configList8247 '())
    ;; (common-do pgm offline-fail (lambda (h f) (f)))
    ;; (common-do pgm offline-fail just-hist)
    ;; (common-do pgm offline-fail just-succeed)
    ;; (common-do pgm offline-fail (lambda (h f) #f))
    (pgm '() offline-fail #f) ;; -- this works except for :nil
    ;; (pgm '() offline-fail (lambda (h f c) #f)) -- works for nil and 1 action
    ;; (pgm '() offline-fail 'halt)
    ;; (pgm '() offline-fail fail-final)
    ;; (pgm '() offline-fail :nil)
    configList8247 ))


;; set transition for offline then evaluate 
(define-macro (with-offline-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;; set transition and online monitors then evaluate
(define-macro (with-online-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-online)
       (start-ergo-monitors)
       (begin0 (call-with-exception-handler ergo-abort 
                 (lambda () ,expr (stop-ergo-monitors)))
               (set! ergo-trans ,trans)))))

;; set transition and online monitors then evaluate
(define-macro (with-online-synchronized-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-online-synchronized)
       (start-ergo-monitors)
       (begin0 (call-with-exception-handler ergo-abort 
                 (lambda () ,expr (stop-ergo-monitors)))
               (set! ergo-trans ,trans)))))

;; set transition for offlineStepAsk then evaluate 
(define-macro (with-offline-step-ask-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline-step-ask)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;; set transition for offlineStep then evaluate 
(define-macro (with-offline-step-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline-step)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;; set transition for offlineStepColl then evaluate 
(define-macro (with-offline-step-coll-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline-collect-step)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;; set transition for offlineStepNatch then evaluate 
(define-macro (with-offline-step-match-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline-step-match)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the body of ergo-do-fn

(unless (and (procedure? pgm) (= (procedure-arity pgm) 3))
  (error "Argument to ergo-do is not an ERGO program"))
(ergo-begin-atomic! #t)   ; initialize begin sequence to atomic
(save-state-excursion (case mode
    ((offline) (with-offline-xeq (common-do pgm offline-fail try-final)))
    ((first) (with-offline-xeq (common-do pgm offline-fail just-hist)))
    ((online) (with-online-xeq (common-do pgm online-fail online-done)))
    ((count) (with-offline-xeq (counting-do pgm)))
    ((offlineStepAsk) (with-offline-step-ask-xeq (common-do pgm offline-fail try-final))) ; new
    ((collecting) (with-offline-xeq (collecting-do pgm))) ; new
    ((offlineStep) (with-offline-step-xeq (common-do pgm offline-fail fail-final))) ; new
    ((offlineStepColl) (with-offline-step-coll-xeq (collecting-step-do pgm))) ; new
    ;; ((offlineStepMatch) (with-offline-step-match-xeq (common-do pgm offline-fail fail-final))) ; new
    ((offlineStepMatch) (with-offline-step-match-xeq  (pgm '() offline-fail #f))) ;new
   ;; ((offlineStepMatch) (with-offline-step-match-xeq  (common-do pgm offline-fail fail-final))) ;new
    ((onlineSynchronized) (with-online-synchronized-xeq (common-do pgm online-fail online-done))) ; new
    (else (error "~a is incorrect mode of execution" mode))))
); ergo-do-fn
