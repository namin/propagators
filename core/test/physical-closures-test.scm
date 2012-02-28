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
 physical-closures

 (define-test (neanderthalic-double)
   (interaction
    (initialize-scheduler)
    (define-cell double
      (make-closure
       (lambda (x out)
	 (p:+ x x out))
       '()))

    (define-cell x 2)
    (define-cell output)
    (p:application double x output)
    (run)
    (content output)
    (produces 4)

    ;; Stable under kicks:
    (alert-all-propagators!)
    (run)
    (content output)
    (produces 4)
    ))

 (define-test (double)
   (interaction
    (initialize-scheduler)
    (define-propagator (double x out)
      (p:+ x x out))

    (define-cell x 2)
    (define-cell output)
    (p:double x output)
    (run)
    (content output)
    (produces 4)

    ;; Stable under kicks:
    (alert-all-propagators!)
    (run)
    (content output)
    (produces 4)
    ))

 (define-test (double-again)
   (interaction
    (initialize-scheduler)
    (define-e:propagator (ce:double x)
      (ce:* x 2))

    (define-cell answer (ce:double %% 2))
    (run)
    (content answer)
    (produces 1)

    ;; Stable under kicks:
    (alert-all-propagators!)
    (run)
    (content answer)
    (produces 1)
    ))

 (define-test (addn)
   (interaction
    (initialize-scheduler)
    (define-cell addn
      (make-closure
       (lambda (n out)
	 ((p:constant
	   (make-closure
	    (lambda (x out)
	      (p:+ n x out))
	    (list n)))
	  out))
       '()))

    (define-cell n 5)
    (define-cell add5)
    (p:application addn n add5)

    (define-cell x 3)
    (define-cell x-output)
    (p:application add5 x x-output)

    (define-cell y 5)
    (define-cell y-output)
    (p:application add5 y y-output)
    
    (run)
    (content x-output)
    (produces 8)
    (content y-output)
    (produces 10)
    
    ;; Stable under kicks:
    (alert-all-propagators!)
    (run)
    (content x-output)
    (produces 8)
    (content y-output)
    (produces 10)
    ))

 (define-test (neanderthalic-merge-addn)
   (interaction
    (initialize-scheduler)
    
    (define-cell addn
      (make-e:closure
       (lambda (n)
	 (e:constant
	  (make-e:closure
	   (lambda (x)
	     (e:+ n x))
	   (list n))))
       '()))

    (define-cell n1 (make-interval 3 5))
    (define-cell n2 (make-interval 4 7))
    (define-cell add5 (e:application addn n1))
    (p:application addn n2 add5)
    
    (define-cell output (e:application add5 3))
    
    (run)
    (content output)
    (produces #(interval 7 8))

    (add-content n2 (make-interval 5 9))
    (run)
    (content output)
    (produces 8)
    ))

 (define-test (merge-addn)
   (interaction
    (initialize-scheduler)
    
    (define-e:propagator (e:addn n)
      (lambda-e:propagator (x)
	(import n)
	(e:+ n x)))

    (define-cell n1 (make-interval 3 5))
    (define-cell n2 (make-interval 4 7))
    (define-cell add5 (e:addn n1))
    (p:addn n2 add5)
    
    (define-cell output (e:application add5 3))
    
    (run)
    (content output)
    (produces #(interval 7 8))

    (add-content n2 (make-interval 5 9))
    (run)
    (content output)
    (produces 8)
    ))

 (define-test (compose)
   (interaction
    (initialize-scheduler)
    (define-cell double
      (make-e:closure
       (lambda (x)
	 (e:+ x x))
       '()))
    (define-cell square
      (make-e:closure
       (lambda (x)
	 (e:* x x))
       '()))
    (define-cell compose
      (make-e:closure
       (lambda (f g)
	 (e:constant
	  (make-e:closure
	   (lambda (x)
	     (e:application f (e:application g x)))
	   (list f g))))
       '()))
    (define-cell double-square (e:application compose double square))
    (define-cell square-double (e:application compose square double))
    
    (define-cell x 2)
    (define-cell 2x^2 (e:application double-square x))
    (define-cell 4x^2 (e:application square-double x))
    
    (run)
    (content 2x^2)
    (produces 8)
    (content 4x^2)
    (produces 16)
    
    ;; Stable under kicks:
    (alert-all-propagators!)
    (run)
    (content 2x^2)
    (produces 8)
    (content 4x^2)
    (produces 16)
    ))

 (define-test (repeat)
   (interaction
    (initialize-scheduler)
    (define-e:propagator (e:double x)
      (e:+ x x))
    (define-e:propagator (e:compose f g)
      (lambda-e:propagator (x)
	(import f g)
	(e:application f (e:application g x))))
    (define-cell repeat
      (let-cell (repeat)
	((constant
	  (lambda-d:propagator (f n out)
	    (import e:compose repeat)
	    (let-cell (repeat? (e:> n 1))
	      (let-cell (done? (e:not repeat?))
		(switch done? f out)
		(let-cells ((n-1 (e:- n 1))
			    fn-1 f-again out-again n-1-again compose-again repeat-again)
		  (switch repeat? n-1 n-1-again)
		  (switch repeat? f f-again)
		  (switch repeat? out-again out)
		  (switch repeat? e:compose compose-again)
		  (switch repeat? repeat repeat-again)
		  (p:application compose-again fn-1 f-again out-again)
		  (p:application repeat-again f-again n-1-again fn-1))))))
	 repeat)
	repeat))

    (define-cell output
      (e:application
       (e:application repeat e:double 4) 2))

    (run)
    (content output)
    (produces 32)
    ))

  (define-test (tms-addn)
   (interaction
    (initialize-scheduler)
    
    (define-propagator (addn n out)
      ((p:constant
	(lambda-d:propagator (x out)
	  (import n)
	  (p:+ n x out)))
       out))

    (define-cell add5-fred (e:application p:addn (make-interval 3 5)))
    (define-cell bill (make-interval 4 7))
    (define-cell add5-bill (e:application p:addn bill))

    (define-cell add5)
    (p:switch (make-tms (supported #t '(fred))) add5-fred add5)
    (p:switch (make-tms (supported #t '(bill))) add5-bill add5)

    (define-cell output (e:application add5 (make-tms (supported 3 '(joe)))))
    
    (run)
    (tms-query (content output))
    (produces #(supported #(interval 7 8) (joe fred bill)))

    (kick-out! 'bill)
    (run)
    (tms-query (content output))
    (produces #(supported #(interval 6 8) (joe fred)))

    (kick-out! 'fred)
    (run)
    (tms-query (content output))
    (produces nothing)
    
    (bring-in! 'bill)
    (run)
    (tms-query (content output))
    (produces #(supported #(interval 7 10) (joe bill)))
    
    (add-content bill (make-tms (supported (make-interval 5 9) '(harry))))
    (run)
    (tms-query (content output))
    (produces #(supported #(interval 8 10) (harry joe bill)))
    ))

  (define-test (first-class-primitives)
    (initialize-scheduler)
    (define-cell output (e:application p:+ 3 4))
    (run)
    (check (= 7 (content output))))

  (define-test (first-class-e:primitives)
    (initialize-scheduler)
    (define-cell output (e:application e:+ 3 4))
    (run)
    (check (= 7 (content output))))

  (define-test (first-class-macro-primitives)
    (initialize-scheduler)
    (define-cell x)
    (define-cell output (e:application c:+ 3 x))
    (add-content output 7)
    (run)
    (check (= 4 (content x))))

  (define-test (manual-example-unknown-operation)
    (interaction
     (initialize-scheduler)
     (define-cell operation)
     (define-cell answer)
     (operation 3 4 answer)
     (run)
     (content answer)
     (produces nothing)
     (p:id p:* operation)
     (run)
     (content answer)
     (produces 12)))

  (define-test (manual-example-tms-apply)
    (interaction
     (initialize-scheduler)
     (define-cell the-operation)
     (define-cell the-answer (e@ the-operation 3 4))
     (p:switch (make-tms (contingent #t '(bill))) e:+ the-operation)
     (run)
     (tms-query (content the-answer))
     (produces #(supported 7 (bill)))
     (kick-out! 'bill)
     (tms-query (content the-answer))
     (produces nothing)
     (p:switch (make-tms (contingent #t '(fred))) e:* the-operation)
     (run)
     (tms-query (content the-answer))
     (produces #(supported 12 (fred)))
     ))

  (define-test (first-class-primitives-tms)
    (interaction
     (initialize-scheduler)
     (define-cell bill-op p:+)
     (define-cell fred-op e:*)
     (define-cell the-op)
     (switch (make-tms (supported #t '(bill))) bill-op the-op)
     (switch (make-tms (supported #t '(fred))) fred-op the-op)
     (define-cell output (e:application the-op 3 4))
     (run)
     (produces '(contradiction (fred bill)))
     (check (equal? '(application) (map name (neighbors the-op))))

     (kick-out! 'bill)
     (run)
     (tms-query (content output))
     (produces #(supported 12 (fred)))
     (check (equal? '(equivalent-closures?:p application)
		    (map name (neighbors the-op))))

     (kick-out! 'fred)
     (bring-in! 'bill)
     (run)
     (tms-query (content output))
     (produces #(supported 7 (bill)))
     (check (equal? '(equivalent-closures?:p equivalent-closures?:p application)
		    (map name (neighbors the-op))))
     ))

  (define-test (returning-e:-vs-p:)
    (interaction
     (initialize-scheduler)
     (define-e:propagator (addn n)
       (define-e:propagator (the-adder x)
	 (import n)
	 (e:+ n x))
       e:the-adder)
     (define-cell answer ((e:addn 3) 2))
     (run)
     (content answer)
     (produces 5)))

  (define-test (returning-p:-vs-e:)
    (interaction
     (initialize-scheduler)
     (define-e:propagator (addn n)
       (define-e:propagator (the-adder x)
	 (import n)
	 (e:+ n x))
       p:the-adder)
     (define-cell answer)
     ((e:addn 3) 2 answer)
     (run)
     (content answer)
     (produces 5)))

  )
