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
 virtual-environments

 (let* ((base (make-frame '()))
	(derived (make-frame (list base)))
	(base-content (make-interval 3 10))
	(derived-content (make-interval 5 15))
	(base-only
	 (alist->virtual-copies `((,base . ,base-content))))
	(derived-only
	 (alist->virtual-copies `((,derived . ,derived-content))))
	(base-and-derived
	 (alist->virtual-copies
	  `((,base . ,base-content)
	    (,derived . ,derived-content)))))
   (define-each-check
     (equal? (list derived base) (frame-ancestors derived))
     (eq? base-content (direct-frame-content base-and-derived base))
     (eq? derived-content (direct-frame-content base-and-derived derived))
     (eq? base-content (full-frame-content base-and-derived base))
     (equivalent? (merge base-content derived-content)
		  (full-frame-content base-and-derived derived))
     (nothing? (direct-frame-content base-and-derived (make-frame '())))
     (nothing? (full-frame-content base-and-derived (make-frame '())))

     (lexical-invariant? base-only)
     (lexical-invariant? derived-only)
     (not (lexical-invariant? base-and-derived))
     (acceptable-frame? derived (list base-only derived-only))
     (good-frame? derived (list base-only derived-only))

     (v-c-equal? base-and-derived (merge base-only derived-only))
     (eq? base-and-derived (merge base-only base-and-derived))
     (eq? base-and-derived
	  (merge (make-virtual-copies `((,base . ,(make-interval 2 12))))
		 base-and-derived))

     (generic-match `((,base . #(interval 9 100)))
       ((v-c-i/o-unpacking generic-square) base-only base-only))
     (generic-match `((,derived . #(interval 9 100)))
       ((v-c-i/o-unpacking generic-square)
	base-only derived-only))
     (generic-match `((,derived . #(interval 25 225)))
       ((v-c-i/o-unpacking generic-square)
	derived-only derived-only))
     (generic-match `((,base . #(interval 25 225)))
       ((v-c-i/o-unpacking generic-square)
	derived-only base-only))

     (generic-match `((,base . #(interval 9 100)))
       ((v-c-i/o-unpacking generic-*)
	base-only base-only base-only))
     (generic-match `((,base . #(interval 15 150)))
       ((v-c-i/o-unpacking generic-*)
	base-only derived-only base-only))
     (generic-match `((,derived . #(interval 15 150)))
       ((v-c-i/o-unpacking generic-*)
	derived-only base-only derived-only))
     (generic-match `((,derived . #(interval 9 100)))
       ((v-c-i/o-unpacking generic-*)
	base-only base-only derived-only))
     ))

 (define-test (interior-propagator-smoke)
   (initialize-scheduler)
   (define-cell four)
   (define-cell zero)
   (define-cell same)
   (vc:=? four zero same)

   (define repl-frame (make-frame '()))
   (add-content four (alist->virtual-copies `((,repl-frame . 4))))
   (add-content zero (alist->virtual-copies `((,repl-frame . 0))))
   (add-content same (alist->virtual-copies `((,repl-frame . ,nothing))))
   (run)
   (content same)
   (check (generic-match `((,repl-frame . #f)) (content same)))
   )

 (define-test (call-site-smoke)
   (initialize-scheduler)
   (define-cell in-squaree)
   (define-cell in-square)
   (vc:squarer in-squaree in-square)

   (define repl-frame  (make-frame '()))
   (define-cell out-squaree)
   (define-cell out-square)
   (static-call-site 
    (make-v-closure (list in-squaree in-square) '() '())
    (list out-squaree out-square))
   (add-content out-squaree
     (alist->virtual-copies `((,repl-frame . 4))))
   (add-content out-square
     (alist->virtual-copies `((,repl-frame . ,nothing))))
   (run)
   (check (generic-match `((,repl-frame . 16)) (content out-square))))

 (define-test (factorial)
   (initialize-scheduler)

   ;; Definition of factorial
   (define-cell in-n)
   (define-cell in-n!)

   (define-cell zero)
   (define-cell control)
   (define-cell not-control)
   (define-cell one)
   (define-cell n-again)
   (define-cell n-1)
   (define-cell n-1!)
   (define-cell empty)

   (define fact
     (make-v-closure
      (list in-n in-n!)
      (list zero control not-control one n-again n-1 n-1! empty)
      '()))				; No global environment yet

   ((vc:const 0) zero)
   ((vc:const 1) one)
   (vc:=? in-n zero control)
   (vc:inverter control not-control)
   (vc:switch control one in-n!)
   (vc:switch not-control in-n n-again)
   (vc:subtractor n-again one n-1)
   (static-call-site fact (list n-1 n-1!))
   (vc:multiplier n-1! in-n in-n!)

   ;; Use
   (define repl-frame  (make-frame '()))
   (define-cell out-n)
   (define-cell out-n!)
   (static-call-site fact (list out-n out-n!))
   (add-content out-n  (alist->virtual-copies `((,repl-frame . 4))))
   (add-content out-n! (alist->virtual-copies `((,repl-frame . ,nothing))))
   (run)
   (check (generic-match `((,repl-frame . 24)) (content out-n!)))
   )

 (define-test (iterative-factorial)
   ;; TODO Of course, for this to really be iterative, we need to
   ;; flatten chains of virtual bridges that have no further effect
   ;; (except passing things through switches and such).  An
   ;; approximation of the fully-determined? predicate might be
   ;; helpful.
   (initialize-scheduler)

   ;; Definition of iterative factorial loop
   (define-cell in-accum)
   (define-cell in-n)
   (define-cell out)

   (define-cell one)
   (define-cell done)
   (define-cell not-done)
   (define-cell recur-accum)
   (define-cell accum-again)
   (define-cell n-again)
   (define-cell out-again)
   (define-cell n-1)

   (define fact-iter-loop
     (make-v-closure
      (list in-accum in-n out)
      (list one done not-done recur-accum accum-again n-again out-again n-1)
      '())) 				; No global environment yet

   ((vc:const 1) one)
   (vc:=? in-n one done)
   (vc:inverter done not-done)
   (vc:switch done in-accum out)
   (vc:switch not-done in-accum accum-again)
   (vc:switch not-done in-n n-again)
   (vc:switch not-done out-again out)
   (vc:subtractor n-again one n-1)
   (vc:multiplier accum-again n-again recur-accum)
   (static-call-site fact-iter-loop (list recur-accum n-1 out-again))

   ;; Definition of iterative factorial start
   (define-cell n)
   (define-cell n!)
   (define-cell init-accum)
   (define fact-start
     (make-v-closure (list n n!) (list init-accum) '()))

   ((vc:const 1) init-accum)
   (static-call-site fact-iter-loop (list init-accum n n!))

   ;; Use
   (define repl-frame (make-frame '()))
   (define-cell my-n)
   (define-cell my-n!)
   (static-call-site fact-start (list my-n my-n!))
   (add-content my-n  (alist->virtual-copies `((,repl-frame . 5))))
   (add-content my-n! (alist->virtual-copies `((,repl-frame . ,nothing))))
   (run)
   (check (generic-match `((,repl-frame . 120)) (content my-n!)))
   )

 (define-test (fibonacci)
   (interaction
    (initialize-scheduler)
    ;; Definition of fibonacci
    (define-cell in-n)
    (define-cell fib-n)
   
    (define-cell one)
    (define-cell two)
    (define-cell recur)
    (define-cell not-recur)
    (define-cell n-again)
    (define-cell n-1)
    (define-cell n-2)
    (define-cell fib-n-1)
    (define-cell fib-n-2)
   
    (define fib
      (make-v-closure
       (list in-n fib-n)
       (list one two recur not-recur n-again n-1 n-2 fib-n-1 fib-n-2)
       '()))

    ((vc:const 1) one)
    ((vc:const 2) two)
    (vc:<? in-n two not-recur)
    (vc:inverter not-recur recur)
    (vc:switch not-recur one fib-n)
    (vc:switch recur in-n n-again)
    (vc:subtractor n-again one n-1)
    (static-call-site fib (list n-1 fib-n-1))
    (vc:subtractor n-again two n-2)
    (static-call-site fib (list n-2 fib-n-2))
    (vc:adder fib-n-1 fib-n-2 fib-n)

    ;; Use
    (define repl-frame (make-frame '()))
    (define-cell my-n)
    (define-cell my-fib-n)
    (static-call-site fib (list my-n my-fib-n))
    (add-content my-n  (alist->virtual-copies `((,repl-frame . 5))))
    (add-content my-fib-n (alist->virtual-copies `((,repl-frame . ,nothing))))
    (run)
    (content my-fib-n)
    (produces `((,repl-frame . 8)))
    ))

 (define-test (fibonacci-again)
   (interaction
    (initialize-scheduler)

    (define repl-frame (make-frame '()))
    (define-cell n)
    (define-cell fib-n)
    (static-call-site fib-cl (list n fib-n))
    (add-content n  (alist->virtual-copies `((,repl-frame . 4))))
    (add-content fib-n (alist->virtual-copies `((,repl-frame . ,nothing))))
    (run)
    (content fib-n)
    (produces `((,repl-frame . 5)))
    ))

 (define-test (euclid)
   (interaction
    (initialize-scheduler)

    (define repl-frame (make-frame '()))
    (define-cell a)
    (define-cell b)
    (define-cell gcd-a-b)
    (static-call-site euclid-cl (list a b gcd-a-b))
    (add-content a (alist->virtual-copies `((,repl-frame . ,(* 17 3)))))
    (add-content b (alist->virtual-copies `((,repl-frame . ,(* 17 5)))))
    (add-content gcd-a-b (alist->virtual-copies `((,repl-frame . ,nothing))))
    (run)
    (content gcd-a-b)
    (produces `((,repl-frame . 17)))
    ))
)
