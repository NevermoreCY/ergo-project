;; author: YL
;; date: 10 Nov 2019

;; This file defines (ergo-trans-match a prog) which checks whether prog
;; can perform a single transition to produce action a;
;; if it can, it returns the remaining program afterwards (only the first
;; remaining program it finds is returned);
;; if it cannot, it returns #f.
;;

;; (subev var val prog)  returns prog with all free occurrences of var
;; substituted by val
;;
;; superseeded by  (subealist alist prog)
;;
;; (define (subev var val prog)
;;   (if (symbol? prog) (if (eq? prog var) val prog)
;;       (if (list? prog)
;; 	  (if (memq (car prog) '(:let let let*))
;; 	      (if (assq var (cadr prog))
;; 		  prog
;; 		  (cons (car prog)
;; 			(cons (cadr prog)
;; 			(map (lambda (p) (subev var val p)) (cddr prog)))))
;; 	      ;;(list var val prog)
;; 	      (map (lambda (p) (subev var val p)) prog)
;; 	      )
;; 	  prog
;;       )
;;       )
;;   )

;; (subealist alist prog) where is a list of pairs (var val) returns
;; prog with all free occurrences of each var simultanously substituted by val

(define (subealist alist prog)
  (if (symbol? prog) (if (assq prog alist)  (cadr (assq prog alist))  prog)
      (if (list? prog)
	  (if (memq (car prog) '(:let let let*))
	      (let* ((newalist (filter (lambda (pair) (not (assq (car pair) (cadr prog)))) alist))
		     (subbody (map (lambda (p) (subealist newalist p)) (cddr prog))))
		(cons (car prog) (cons (cadr prog) subbody)))
	      (map (lambda (p) (subealist alist p)) prog)
	      )
	  prog
      )
      )
  )

;; final? returns #t iff prog is final, i.e. can legally terninate
;; note that a (:test fexpr) is considered final
;; if fexpr does not evaluate to #f

(define (final? prog)
  (if (eq? prog ':nil) #t
      (if (eq? prog ':fail) #f
	  (case (car prog)
	    	[(:test) (if (eval (cadr prog)) #t #f)]
	[(:act) #f]
	[(:choose) (or-map (lambda (x) (final? x)) (cdr prog))]
	[(:begin) (and-map (lambda (x) (final? x)) (cdr prog))]
	[(:star) #t]
	[(:if) (if (eval (cadr prog))
		   (final? (caddr prog))
		   (final? (cadddr prog)))
	 ]
	[(:while) (or (not (eval (cadr prog)))
		      (final? (cons ':begin (cddr prog))))]
	[(:let)
	  (let* ((alist (cadr prog))
		 (alistev (map (lambda (pair) (list (car pair) (eval (cadr pair)))) alist)))
	    (final? (subealist alistev (cons ':begin (cddr prog)))))]
	[(:for-some)  (let ((var (cadr prog))
				    (vallist (caddr prog))
				    (body (cdddr prog)))
				(if (null? vallist) #f
				    (final? (list ':choose (cons ':let (cons (list (list var (car vallist))) body)) (cons ':for-some (cons var (cons (cdr vallist) body)))))))]
	)
      )
      )
  )


;; we want to handle the new construct (:for-some-in-act varlist test act)

(define (ergo-trans-match a prog)
  (if (eq? prog ':nil)
      #f
      (if (eq? prog ':fail) #f
	  (case (car prog)
	    	[(:test) #f]
		[(:act) (let ((aa (eval (cadr prog)))) 
		  (if (and (equal? a aa) (legal-action? a)) ':nil #f))]
		[(:begin) (if (null? (cddr prog))
		       (ergo-trans-match a (cadr prog))
		       (let ((f (ergo-trans-match a (cadr prog))))
			 (if f
			     (if (eq? f ':nil)
				 (if (null? (cdddr prog))
					    (caddr prog)
					    (cons ':begin (cddr prog)))
				 (cons ':begin (cons f (cddr prog))))
			     (and (final? (cadr prog))
				  (ergo-trans-match a (cons ':begin (cddr prog)))))))]
		;; [(:begin2) (let ((f (ergo-trans-match a (cadr prog))))
		;; 	     (if f
		;; 		 (if (eq? f ':nil)  (caddr prog)
		;; 		     (cons ':begin2 (cons f (cddr prog))))
		;; 		 (and (final? (cadr prog)) (ergo-trans-match a (caddr prog)))))]
		[(:choose) (if (null? (cddr prog))
		       (ergo-trans-match a (cadr prog))
		       (let ((f (ergo-trans-match a (cadr prog))))
			 (if f
			     f
			     (ergo-trans-match a (cons ':choose (cddr prog))))))]
		;; [(:choose2)
		;;  (or (ergo-trans-match a (cadr prog))
		;; 		(ergo-trans-match a (caddr prog)))]
		;; [(:for-some)  (if (null? (cdaddr prog))
		;; 		  (ergo-trans-match a (subv (caddr prog) (eval (caadddr prog))))
		;; 		  (ergo-trans-match a (cons ':choose (subv (caddr prog) (eval (caadddr prog))) (cons ':for-some (cadr prog) (cdaddr prog) (cadddr prog)))))]
		;; [(:for-some-in-act) ()
		[(:star) (ergo-trans-match a (list ':begin  (cadr prog) prog))]
		[(:if) (if (eval (cadr prog))
			   (ergo-trans-match a (caddr prog))
			   (ergo-trans-match a (cadddr prog)))
		 ]
		[(:while) (if (not (eval (cadr prog)))
			      #f
			      (let ((rbody (ergo-trans-match a (cons ':begin (cddr prog)))))
				(list ':begin rbody prog)))]
		[(:let)
		 (let* ((alist (cadr prog))
		       (alistev (map (lambda (pair) (list (car pair) (eval (cadr pair)))) alist)))
		   (ergo-trans-match a (subealist alistev (cons ':begin (cddr prog)))))]
		;; [(:let) (let* ((bindings (map (lambda (p) (list (car p) (eval (cadr p)))) (cadr prog)))
		;; 	       (res (eval (list 'let bindings  (list 'ergo-trans-match (list 'quote a) (cons ':begin (cddr prog)))))))
		 ;; 	  (if res (list ':let bindings res) res))]
		[(:for-some)  (let ((var (cadr prog))
				    (vallist (caddr prog))
				    (body (cdddr prog)))
				(if (null? vallist) #f
				    (ergo-trans-match a (list ':choose (cons ':let (cons (list (list var (car vallist))) body)) (cons ':for-some (cons var (cons (cdr vallist) body))))))) ]
		)
	  )
      )
  )

;; returns the remaining program after executing prog to generate actlist

(define (ergo-trans-matchseq actlist prog)
  (if (null? actlist) prog
      (ergo-trans-matchseq (cdr actlist)
			   (ergo-trans-match (car actlist) prog))))

  
