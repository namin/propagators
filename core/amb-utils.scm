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

(define-propagator (require cell)
  ((constant #t) cell))
(define require p:require)

(define-propagator (forbid cell)
  ((constant #f) cell))
(define forbid p:forbid)

(define-propagator-syntax (require-distinct cells)
  (for-each-distinct-pair
   (lambda (c1 c2)
     (forbid (e:eqv? c1 c2)))
   cells))

(define-propagator-syntax (one-of . cells)
  (let ((output (ensure-cell (car (last-pair cells))))
	(inputs (map ensure-cell (except-last-pair cells))))
    (cond ((= (length inputs) 2)
	   (conditional (e:amb) (car inputs) (cadr inputs) output))
	  ((> (length inputs) 2)
	   (conditional (e:amb) (car inputs)
			(apply e:one-of (cdr inputs)) output))
	  (else
	   (error "Inadequate choices for one-of"
		  inputs output)))))
(propagator-constructor! one-of)
(define p:one-of one-of)
(define e:one-of (expression-style-variant one-of))

(define p:amb binary-amb)
(define (e:amb)
  (let ((answer (make-named-cell (generate-cell-name))))
    (binary-amb answer)
    (eq-put! answer 'subexprs '())
    answer))
