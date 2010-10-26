;;; ----------------------------------------------------------------------
;;; Copyright 2009 Massachusetts Institute of Technology.
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

(declare (usual-integrations make-cell cell?))

;;;; Compound data

;;; The code for merging compound data turns out not to depend on the
;;; choice between the "copying data" or "carrying cells" strategies
;;; --- those are dependent entirely on what the constructor
;;; and accessor propagators do.

;;; Cons looks like this:
#|
 (define (pair-equivalent? pair1 pair2)
   (and (equivalent? (car pair1) (car pair2))
	(equivalent? (cdr pair1) (cdr pair2))))

 (define (pair-merge pair1 pair2)
   (effectful-bind (merge (car pair1) (car pair2))
     (lambda (car-answer)
       (effectful-bind (merge (cdr pair1) (cdr pair2))
	 (lambda (cdr-answer)
	   (cons car-answer cdr-answer))))))

 (defhandler merge pair-merge pair? pair?)
 (defhandler equivalent? pair-equivalent? pair? pair?)
|#

;;; The generalization to arbitrary product types:

(define (slotful-information-type predicate? constructor . accessors)
  (define (slotful-equivalent? thing1 thing2)
    (apply boolean/and
     (map (lambda (accessor)
	    (equivalent? (accessor thing1) (accessor thing2)))
	  accessors)))
  (define (slotful-merge thing1 thing2)
    (let* ((slots1 (map (lambda (accessor) (accessor thing1))
			accessors))
	   (slots2 (map (lambda (accessor) (accessor thing2))
			accessors)))
      (effectful-list-bind (map merge slots1 slots2)
	(lambda (submerges)
	  (apply constructor submerges)))))
  (define (slotful-contradiction? thing)
    (any contradictory? (map (lambda (accessor) (accessor thing)) accessors)))
  (defhandler merge slotful-merge predicate? predicate?)
  (defhandler equivalent? slotful-equivalent? predicate? predicate?)
  (defhandler contradictory? slotful-contradiction? predicate?))

(slotful-information-type pair? cons car cdr)

