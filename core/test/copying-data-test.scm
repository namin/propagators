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

;;; The "copying data" strategy from the thesis is given by these
;;; definitions of the cons-car-cdr propagators:
#|
 (define conser (function->propagator-constructor cons))
 (define carer (function->propagator-constructor (nary-unpacking car)))
 (define cdrer (function->propagator-constructor (nary-unpacking cdr)))
|#
;;; This strategy is tested here, with the definitions in question
;;; appearing inside the test scope below.

;;; The "carrying cells" strategy is elaborated in
;;; extensions/carrying-cells.scm.  Since the merging is the same in
;;; both cases, the two strategies may be intermixed within the same
;;; network --- just make sure your propagators know what to expect
;;; (and there is as yet no good story for merging a piece of data and
;;; a cell, so merging a carrying cons with a copying cons will not do
;;; anything good).

(in-test-group
 copying-data

 (define-test (example)
   (interaction
    (define conser (function->propagator-constructor cons))
    (define carer (function->propagator-constructor (nary-unpacking car)))
    (define cdrer (function->propagator-constructor (nary-unpacking cdr)))

    (initialize-scheduler)

    (define-cell x)
    (define-cell y)
    (define-cell pair)
    (conser x y pair)

    (run)
    (content pair)
    (produces '( #(*the-nothing*) . #(*the-nothing*) ))

    (define-cell control)
    (define-cell switched-pair)
    (switch control pair switched-pair)

    (add-content control (make-tms (supported #t '(joe))))
    (run)
    (content switched-pair)
    (produces #(tms (#(supported ( #(*the-nothing*) . #(*the-nothing*) ) (joe)))))

    (define-cell x-again)
    (carer switched-pair x-again)

    (run)
    (content x-again)
    (produces #(*the-nothing*))

    (add-content x (make-tms (supported 4 '(harry))))

    (run)
    (content pair)
    (produces '( #(tms (#(supported 4 (harry)))) . #(*the-nothing*) ))

    (content switched-pair)
    (produces #(tms (#(supported ( #(tms (#(supported 4 (harry)))) . #(*the-nothing*) )
				 (joe)))))

    (content x-again)
    (produces #(tms (#(supported 4 (harry joe)))))
    )))
