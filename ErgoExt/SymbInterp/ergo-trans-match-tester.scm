;; author: YL
;; date: 10 Nov 2019

;; A tester program for ergo-trans-match
;;
;; to run, enter:
;; "racket -l ergoExt -i -f ergo-trans-match-tester.scm"
;; and then enter "(ergo-trans-match-tester)" at the Scheme prompt


(include "ergo-trans-match.scm")


;; A simple Ergo domain for testing

;; Fluents
(define-fluents  
  p  #f
  k  1
  )

;; Actions
(define-action a)

(define-action b
  p #t)

(define-action c)

(define-action d
	       #:prereq	p
	       )

(define-action (e i))

(define-action (incr i)
  k (+ k i)
  )

;; testing functions

;; set verbose to #t for tester to print detailed info on tests done

(define verbose #f)

;; test-case evaluates expr and checks whether it returns val
;; if it does, the function returns #t and if verbose is #t it prints a message
;; if it does not, the function returns #f and prints a message

(define (test-case expr val)
  (let* ((res (eval expr)) (succeeds (equal? res val)))
    (when (not succeeds)
	(printf "test case fails: ~a returns ~a instead of ~a\n" expr res val))
    (when (and verbose succeeds)
	(printf "test case succeeds: ~a returns ~a as expected\n" expr res))
    succeeds))

;; A unit tester for subev

;; (define (subev-tester)
;;   (and
;;    (test-case '(subev 'x 3 'x) 3)
;;    (test-case '(subev 'x 3 'y) 'y)
;;    (test-case '(subev 'x 3 4) 4)
;;    (test-case '(subev 'x 3 "a") "a")
;;    (test-case '(subev 'x 3 '+) '+)
;;    (test-case '(subev 'x 3 '(+ x y)) '(+ 3 y))
;;    (test-case '(subev 'x 3 '(:act x y)) '(:act 3 y))
;;    (test-case '(subev 'x 3 '(:begin (:act y) (:act x) (:act x y))) '(:begin (:act y) (:act 3) (:act 3 y)))
;;    (test-case '(subev 'x 3 '(:test (> y x))) '(:test (> y 3)))
;;    (test-case '(subev 'x 3 '(:let ((z 1)) (:act y) (:act x) (:act x y))) '(:let ((z 1)) (:act y) (:act 3) (:act 3 y)))
;;    (test-case '(subev 'x 3 '(:let ((z 1)(x 4)) (:act y) (:act x) (:act x y))) '(:let ((z 1) (x 4)) (:act y) (:act x) (:act x y)))
;;    (test-case '(subev 'x 3 '(:let ((z 1)) (:choose (:act y) (:act x) (:act x y)))) '(:let ((z 1)) (:choose (:act y) (:act 3) (:act 3 y))))
;;    (test-case '(subev 'x 3 '(:let ((z 1)(x 4))(:choose (:act y) (:act x) (:act x y)))) '(:let ((z 1) (x 4)) (:choose (:act y) (:act x) (:act x y))))
;;    (test-case '(subev 'x 3 '(:begin (:let ((z 1)(x 4)) (:act y) (:act x) (:act x y)) (:act x y))) '(:begin (:let ((z 1) (x 4)) (:act y) (:act x) (:act x y)) (:act 3 y)))
;;    ))

;; a unit tester for subealist

(define (subealist-tester)
  (and
   (test-case '(subealist '((x 3)) 'x) 3)
   (test-case '(subealist '((x 3)) 'y) 'y)
   (test-case '(subealist '((x 3)) 4) 4)
   (test-case '(subealist '((x 3)) "a") "a")
   (test-case '(subealist '((x 3)) '+) '+)
   (test-case '(subealist '((x 3)) '(+ x y)) '(+ 3 y))
   (test-case '(subealist '((x 3)) '(:act x y)) '(:act 3 y))
   (test-case '(subealist '((x 3)) '(:begin (:act y) (:act x) (:act x y))) '(:begin (:act y) (:act 3) (:act 3 y)))
   (test-case '(subealist '((x 3)) '(:test (> y x))) '(:test (> y 3)))
   (test-case '(subealist '((x 3)) '(:let ((z 1)) (:act y) (:act x) (:act x y))) '(:let ((z 1)) (:act y) (:act 3) (:act 3 y)))
   (test-case '(subealist '((x 3)) '(:let ((z 1)(x 4)) (:act y) (:act x) (:act x y))) '(:let ((z 1) (x 4)) (:act y) (:act x) (:act x y)))
   (test-case '(subealist '((x 3)) '(:let ((z 1)) (:choose (:act y) (:act x) (:act x y)))) '(:let ((z 1)) (:choose (:act y) (:act 3) (:act 3 y))))
   (test-case '(subealist '((x 3)) '(:let ((z 1)(x 4))(:choose (:act y) (:act x) (:act x y)))) '(:let ((z 1) (x 4)) (:choose (:act y) (:act x) (:act x y))))
   (test-case '(subealist '((x 3)) '(:begin (:let ((z 1)(x 4)) (:act y) (:act x) (:act x y)) (:act x y))) '(:begin (:let ((z 1) (x 4)) (:act y) (:act x) (:act x y)) (:act 3 y)))
   (test-case '(subealist '((x 3) (y 7)) '(:let ((z 1)(x 4))(:choose (:act y) (:act x) (:act x y)))) '(:let ((z 1) (x 4)) (:choose (:act 7) (:act x) (:act x 7))))
   (test-case '(subealist '((x 3) (y 7)) '(:begin (:let ((z 1)(x 4)) (:act y) (:act x) (:act x y)) (:act x y)))
	      '(:begin (:let ((z 1) (x 4)) (:act 7) (:act x) (:act x 7)) (:act 3 7)))
   (test-case '(subealist '((x 3) (y 7)) '(:begin (:act y z) (:act x) (:act x y))) '(:begin (:act 7 z) (:act 3) (:act 3 7)))
   (test-case '(subealist '((x y) (y x)) '(:begin (:act y z) (:act x) (:act x y))) '(:begin (:act x z) (:act y) (:act y x)))
   )
  )

;; a unit tester for final?

(define (final?-tester)
  (and
   (test-case '(final? ':nil) #t)
   (test-case '(final? '(:act a)) #f)
   (test-case '(final? '(:choose (:act a) :nil)) #t)
   (test-case '(final? '(:choose (:act a) (:act b) (:act c))) #f)
   (test-case '(final? '(:choose (:act a) (:act b) (:choose (:act c) :nil))) #t)
   (test-case '(final? '(:begin :nil :nil :nil)) #t)
   (test-case '(final? '(:begin :nil :nil (:act a))) #f)
   (test-case '(final? '(:begin :nil (:choose (:act a) :nil))) #t)
   (test-case '(final? '(:if p (:act a) :nil)) #t)
   (test-case '(final? '(:if (not p) (:act a) :nil)) #f)
   (test-case '(final? '(:test (not p))) #t)
   (test-case '(final? '(:test p)) #f)
   (test-case '(final? '(:let ((x 7)) (:act (e x)) (:act (e x)))) #f)
   (test-case '(final? '(:let ((x 7)) (:test (eq? x 7))))  #t)
   (test-case '(final? '(:let ((x k)) (:test (eq? x 1))))  #t)
   (test-case '(final? '(:let ((x k)) (:test (eq? x k))))  #t)
   (test-case '(final? '(:for-some x (1 3) (:act (e x)) (:act (e x)))) #f)
   (test-case '(final? '(:for-some x (1 3) (:choose (:act (e x)) :nil)))  #t)
   )
  )


;; a tester for ergo-trans-match

(define (ergo-trans-match-tester)
  (and
   (test-case '(ergo-trans-match 'a ':nil) #f)
   (test-case '(ergo-trans-match 'a '(:act a)) ':nil)
   (test-case '(ergo-trans-match '(e 1) '(:act (e 1))) ':nil)
   (test-case '(ergo-trans-match '(e 1) '(:act (e k))) ':nil)
   (test-case '(ergo-trans-match 'a '(:choose (:act a) (:act b))) ':nil)
   (test-case '(ergo-trans-match 'b '(:choose (:act a) (:act b))) ':nil)
   (test-case '(ergo-trans-match 'c '(:choose (:act a) (:act b) (:act c))) ':nil)
   (test-case '(ergo-trans-match 'c '(:choose (:act a) (:act b))) #f)
   (test-case '(ergo-trans-match '(e 1) '(:choose (:act a) (:act b))) #f)
   (test-case '(ergo-trans-match '(e 1) '(:choose (:act a) (:act (e 1)))) ':nil)
   (test-case '(ergo-trans-match 'a '(:begin (:act a) (:act b))) '(:act b))
   (test-case '(ergo-trans-match 'a '(:begin (:choose (:act a) (:act b)) (:act c))) '(:act c))
   (test-case '(ergo-trans-match 'a '(:begin (:act a) (:act b) (:act c))) '(:begin (:act b) (:act c)))
   (test-case '(ergo-trans-match 'a '(:begin (:begin (:act a) (:act b)) (:act c))) '(:begin (:act b) (:act c)))
   (test-case '(ergo-trans-match 'a '(:star (:act a))) '(:star (:act a)))
   (test-case '(ergo-trans-match 'a '(:star (:begin (:act a) (:act b)))) '(:begin (:act b) (:star (:begin (:act a) (:act b)))))
   (test-case '(ergo-trans-match 'a '(:if (not p) (:act a) :nil)) ':nil)
   (test-case '(ergo-trans-match 'a '(:if p (:act a) :nil)) #f)
   (test-case '(ergo-trans-match 'a '(:begin (:test (not p)) (:act a))) ':nil)
   (test-case '(ergo-trans-match '(e 7) '(:let ((x 7)) (:act (e x)) (:act (e x)))) '(:act (e 7)))
   (test-case '(ergo-trans-match '(e 1) '(:let ((x k)) (:act (e x)) (:act (e x)))) '(:act (e 1)))
   (test-case '(ergo-trans-match '(e 1) '(:for-some x (1 3) (:act (e x)) (:act (e x)))) '(:act (e 1)))
   (test-case '(ergo-trans-match '(e 1) '(:for-some x (k 3) (:act (e x)) (:act (e x)))) '(:act (e 1)))
   )
  )
