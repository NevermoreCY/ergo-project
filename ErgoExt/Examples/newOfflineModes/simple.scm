(include "simple-bat.scm")

(define (nondet1)
  (:choose (:act a) (:act b) (:act c)))

(define (nondet2)
  (:begin
   (:choose (:act a) (:act b) (:act c))
   (:act d)))

(define (nondet3)
  (:begin
   (:choose (:act a) (:act b) (:act c))
   (:choose (:act a) (:act b) (:act c))
   (:act d)))

(define (main)
  (ergo-do #:mode 'offline (nondet1)))
