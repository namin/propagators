;;; ----------------------------------------------------------------------
;;; Copyright 2009-2010 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of Propagator Network Prototype.
;;; 
;;; Propagator Network Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;; 
;;; Propagator Network Prototype is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Propagator Network Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(in-test-group
 symbolics-ineq

 (define-each-check
   (generic-match
    (vector 'symb-ineq nothing '((< me 5 (me)) (> me 4 (me))) '())
    (symb-ineq-merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 5))
      '())
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 4))
      '())
     ))

   (generic-match
    (vector 'symb-ineq nothing '((< me 5 (me)) (> me 4 (me))) '())
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 5))
      '())
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 4))
      '())
     ))

   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 5))
      '())
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 6))
      '())
     ))

   (generic-match
    #(symb-ineq #(symbolic 4 #(metadata () () ())) () ())
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 5))
      '())
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata))
      '()
      '())
     ))

   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      nothing
      '()
      (list (%make-inequality '< 'x 5)))
     (make-symb-ineq
      nothing
      '()
      (list (%make-inequality '> 'x 6)))
     ))

   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata))
      '()
      (list (%make-inequality '< 'x 5)))
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata))
      '()
      (list (%make-inequality '> 'x 6)))
     ))

   (generic-match
    #(symb-ineq #(symbolic 4 #(metadata () () ()))
		()
		((< x 5 (x))))
    (merge
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata))
      '()
      (list (%make-inequality '< 'x 5)))
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata))
      '()
      '())
     ))
   
   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      (make-symbolic 4 (empty-metadata)) '() '())
     (make-symb-ineq
      (make-symbolic 5 (empty-metadata)) '() '())
     ))

   (generic-match
    #(symb-ineq #(symbolic v #(metadata (v) () ()))
		()
		((< v 5 (v))))
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 5))
      '())
     (make-symb-ineq
      (variable->symbolic 'v)
      '()
      '())
     ))

   (generic-match
    #(symb-ineq #(symbolic v #(metadata (v) () ()))
		()
		((> v 0 (v))))
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me '(* 2 v)))
      '())
     (make-symb-ineq
      (variable->symbolic 'v)
      '()
      '())
     ))

   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      (variable->symbolic 'v)
      '()
      (list (%make-inequality '> 'v 6)))
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me '2))
      '())
     ))

   (generic-match
    #(symb-ineq #(*the-nothing*)
		((> me v (me v))
		 (< me 2 (me)))
		((< v 2 (v))))
    (merge
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 'v))
      '())
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me '2))
      '())
     ))

   (generic-match
    #(symb-ineq #(symbolic w #(metadata (v w) (((= v (* 4 w)) ())) ()))
		()
		((> w 0 (w))
		 (< w 1/2 (w))))
    (merge
     (make-symb-ineq
      (make-symbolic
       'w
       (make-symbolic-metadata
	'(v w)
	'(((= v (* 4 w)) ()))
	'()))
      '()
      (list (%make-inequality '> 'w 0)))
     (make-symb-ineq
      nothing
      '()
      (list (%make-inequality '< 'v '2)))
     ))

   (generic-match
    #(symb-ineq #(symbolic w #(metadata (v w) (((= v (* 4 w)) ())) ()))
		()
		((> w 0 (w))))
    (merge
     (make-symb-ineq
      (make-symbolic
       'w
       (make-symbolic-metadata
	'(v w)
	'(((= v (* 4 w)) ()))
	'()))
      '()
      (list (%make-inequality '> 'w 0)))
     (make-symb-ineq
      nothing
      (list (%make-inequality '< 'me 'v))
      '())
     ))

   (generic-match
    the-contradiction
    (merge
     (make-symb-ineq
      (make-symbolic
       'w
       (make-symbolic-metadata
	'(v w)
	'(((= v (* 4 w)) ()))
	'()))
      '()
      (list (%make-inequality '> 'w 0)))
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 'v))
      '())
     ))

   (generic-match
    #(symb-ineq #(symbolic w #(metadata (v w) (((= v (* 4 w)) ())) ()))
		()
		((< w 0 (w))))
    (merge
     (make-symb-ineq
      (make-symbolic
       'w
       (make-symbolic-metadata
	'(v w)
	'(((= v (* 4 w)) ()))
	'()))
      '()
      '())
     (make-symb-ineq
      nothing
      (list (%make-inequality '> 'me 'v))
      '())
     ))

   (generic-match #t ;; TODO Should this forward the metadata?
    ((binary-mapping generic-<=)
     (make-symbolic
      -6
      (make-symbolic-metadata '(x) '(((= x 0) ())) '()))
     (make-symbolic
      0
      (make-symbolic-metadata '(x) '(((= x 0) ())) '()))))

   (generic-match #t ;; TODO Should this detect the inconsistency?
    ((binary-mapping generic-<=)
     (make-symbolic
      -6
      (make-symbolic-metadata '(x) '(((= x 6) ())) '()))
     (make-symbolic
      0
      (make-symbolic-metadata '(x) '(((= x 0) ())) '()))))

   (generic-match #t ;; TODO Should this forward the metadata?
    ((binary-mapping generic-<=)
     (make-symb-ineq
      (make-symbolic
       -6
       (make-symbolic-metadata '(x) '(((= x 0) ())) '()))
      '()
      '())
     (make-symb-ineq
      (make-symbolic
       0
       (make-symbolic-metadata '(x) '(((= x 0) ())) '()))
      '()
      '())))

   )

 (define-test (ineq-enforcer-smoke)
   (interaction
    (initialize-scheduler)
    (define-cell five)
    ((constant (make-tms (supported 5 '(joe)))) five)
    (define-cell victim)
    ((ineq-enforcer '<) five victim)
    ((ineq-enforcer '<) victim five)
    ;; Doesn't detect the contradiction without a plunk
    (run)
    (produces 'done)

    (plunker victim)
    (run)
    (produces '(contradiction (joe)))))

 (define-test (ineq-constraint-smoke)
   (interaction
    (initialize-scheduler)
    (define-cell four)
    ((constant 4) four)
    (define-cell ctl)
    (define-cell victim)
    (c:> ctl four victim)
    ((constant #t) ctl)
    (run)
    (content victim)
    (produces (vector 'symb-ineq nothing '((< me 4 (me))) '()))))

 (define-test (more-ineq-constraint-smoke)
   (interaction
    (initialize-scheduler)
    (define-cell three)
    ((constant (make-tms (supported 3 '(joe)))) three)
    (define-cell victim)
    (define-cell ctl)
    (c:< ctl three victim)
    (c:>= ctl three victim)
    ;; Doesn't detect the contradiction without a boolean plunk on the
    ;; control
    (run)
    (produces 'done)

    (binary-amb ctl)
    (run)
    (produces '(contradiction (joe)))))

 (define-test (even-more-ineq-constraint-smoke)
   (interaction
    (initialize-scheduler)
    (define-cell three)
    ((constant (make-tms (supported 3 '(joe)))) three)
    (define-cell victim1)
    (define-cell victim2)
    (define-cell ctl)
    (c:< ctl three victim1)
    (c:>= ctl three victim2)
    (define-cell zero)
    ((constant 0) zero)
    (c:+ zero victim1 victim2)
    ;; Doesn't detect the contradiction without a boolean plunk on the
    ;; control
    (run)
    (produces 'done)

    (binary-amb ctl)
    ;; Doesn't detect the contradiction without a variable plunk, either
    (run)
    (produces 'done)

    (plunker victim1)
    (run)
    (produces '(contradiction (joe)))))

 (define-test (even-more-more-ineq-constraint-smoke)
   (interaction
    (initialize-scheduler)
    (define-cell three)
    ((constant (make-tms (supported 3 '(joe)))) three)
    (define-cell victim1)
    (define-cell victim2)
    (define-cell ctl)
    (c:< ctl three victim1)
    (c:>= ctl three victim2)
    (define-cell zero)
    ((constant 0) zero)
    (c:+ zero victim1 victim2)
    ;; Doesn't detect the contradiction without a boolean plunk on the
    ;; control.
    (run)
    (produces 'done)

    (binary-amb ctl)
    ;; Doesn't detect the contradiction without a variable plunk, either 
    (run)
    (produces 'done)

    (define-cell four)
    ((constant 4) four)
    (define-cell x)
    (plunker x)
    (p:* four x victim1)

    (run)
    (produces '(contradiction (joe)))))
)
