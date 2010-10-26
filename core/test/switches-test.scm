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
 switches

 (define switching-function (nary-unpacking switch-function))

 (define-each-check
   (generic-match nothing (switching-function nothing nothing))
   (generic-match nothing (switching-function nothing 7))
   (generic-match nothing (switching-function #f 7))
   (generic-match 7 (switching-function #t 7))

   (generic-match
    #(supported 7 (bob))
    (switching-function (supported #t '(bob)) 7))
   (generic-match
    #(supported 7 (harry bob))
    (switching-function (supported #t '(bob)) (supported 7 '(harry))))

   (generic-match
    nothing
    (switching-function (supported #t '(bob)) nothing))
   (generic-match
    nothing
    (switching-function (supported #f '(bob)) 7))
   (generic-match
    nothing
    (switching-function (supported #f '(bob)) (supported 7 '(harry))))

   (generic-match
    #(tms (#(supported 7 (bob))))
    (switching-function (make-tms (supported #t '(bob))) 7))
   (generic-match
    nothing
    (switching-function (make-tms (supported #t '(bob))) nothing))
   (generic-match
    #(tms (#(supported 7 (harry bob))))
    (switching-function (make-tms (supported #t '(bob))) (supported 7 '(harry))))
   (generic-match
    #(tms (#(supported 7 (harry bob))))
    (switching-function (make-tms (supported #t '(bob)))
			(make-tms (supported 7 '(harry)))))
   (generic-match
    #(tms (#(supported #(interval 7 8) (harry bob))))
    (switching-function (make-tms (supported #t '(bob)))
			(make-tms (supported (make-interval 7 8) '(harry)))))

   (generic-match
    nothing
    (switching-function (make-tms (supported #f '(bob))) nothing))
   (generic-match
    nothing
    (switching-function (make-tms (supported #f '(bob))) 7))
   (generic-match
    nothing
    (switching-function (make-tms (supported #f '(bob))) (supported 7 '(harry))))
   (generic-match
    nothing
    (switching-function (make-tms (supported #f '(bob)))
			(make-tms (supported 7 '(harry)))))

   (generic-match
    nothing
    (disbelieving 'bob
     (switching-function (make-tms (supported #t '(bob))) 7)))
   (generic-match
    nothing
    (disbelieving 'harry
     (switching-function (make-tms (supported #t '(bob)))
			 (make-tms (supported 7 '(harry))))))
   (generic-match
    nothing
    (disbelieving 'harry
     (switching-function (make-tms (supported #t '(bob)))
			 (make-tms (supported (make-interval 7 8) '(harry))))))

   (generic-match
    #(tms (#(supported 3 (bob joe))))
    (disbelieving 'fred
     (switching-function (make-tms (list (supported #f '(fred))
					 (supported #t '(joe))))
			 (make-tms (supported 3 '(bob))))))

   (generic-match
    nothing
    (disbelieving 'joe
     (switching-function (make-tms (list (supported #f '(fred))
					 (supported #t '(joe))))
			 (make-tms (supported 3 '(bob))))))

   (generic-match
    nothing
    (disbelieving 'fred
     (disbelieving 'bob
      (switching-function (make-tms (list (supported #f '(fred))
					  (supported #t '(joe))))
			  (make-tms (supported 3 '(bob)))))))

   (generic-match
    #(tms (#(supported 4 (harry bob))))
    (switching-function (supported #t '(bob))
			(make-tms (supported 4 '(harry)))))

   (generic-match
    #(tms (#(supported 4 (harry bob))))
    (disbelieving 'fred
     (switching-function (supported #t '(bob))
			 (make-tms (list (supported 4 '(harry))
					 (supported 5 '(fred)))))))

   (generic-match
    #(tms (#(supported 4 (harry bob))))
    (switching-function (supported #t '(bob))
			(make-tms (list (supported 4 '(harry))
					(supported 4 '(fred))))))
   )

 (define-test (propagator)
   (interaction
    (initialize-scheduler)
    (define-cell input)
    (define-cell control)
    (define-cell output)
    (switch control input output)

    (add-content input 4)
    (add-content control (supported #t '(fred)))
    (run)
    (content output)
    (produces #(supported 4 (fred)))
    ))
 
 )

