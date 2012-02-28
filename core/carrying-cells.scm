;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul and Gerald Jay Sussman
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

;;;; Propagators implementing the carrying cells strategy
;;; for compound data structures.

;;; CONS looks like this:
#;
 (define (p:cons a-cell d-cell output)
   ((constant (cons a-cell d-cell)) output))

;;; The general version for arbitrary constructors:

(define (function->cell-carrier-constructor f)
  (propagator-constructor!
   (lambda cells
     (let ((output (ensure-cell (car (last-pair cells))))
	   (inputs (map ensure-cell (except-last-pair cells))))
       (let ((answer-diagram ((constant (apply f inputs)) output)))
	 (execute-propagator   ; To enable the early-access-hack below
	  (diagram-identity
	   answer-diagram))
	 answer-diagram)))))

;;; Type testers like pair? are just normal propagators.
;;; Accessors are just constructors in reverse, like this:
#;
 (define-propagator (p:car pair-cell output)
   (p:cons output nothing pair-cell))

;;; Expression-style accessors offer an opportunity for a performance
;;; hack: if the cell holding the accessed item is already present in
;;; the compound when the accessor propagator is constructed (and no
;;; partialness of information intervenes), then it's ok to just grab
;;; that cell and return it.  The version for CAR looks like this:
#|
 (define (e:carry-car pair-cell)
   (if (and (cell? pair-cell)
	    (pair? (content pair-cell))
	    (cell? (car (content pair-cell))))
       (car (content pair-cell))
       (%e:carry-car pair-cell)))
|#
;;; The general version looks like this:

(define (early-access-hack type? accessor fallback)
  (propagator-constructor!
   (lambda (structure-cell)
     (if (and (cell? structure-cell)
	      (type? (content structure-cell))
	      (cell? (accessor (content structure-cell))))
	 (accessor (content structure-cell))
	 (fallback structure-cell)))))

;;; To actually define those propagators, you would write
#|
 (define-cell p:cons (function->cell-carrier-constructor cons))
 (define-cell e:cons (expression-style-variant p:cons))
 (propagatify pair?)
 (define-propagator (p:car pair-cell output)
   (p:cons output nothing pair-cell))
 (define-propagator (p:cdr pair-cell output)
   (p:cons nothing output pair-cell))
 (define-cell e:car (early-access-hack pair? car e:car))
 (define-cell e:cdr (early-access-hack pair? cdr e:cdr))
|#

;;; That's what the define-propagator-structure macro is for.

(define-syntax define-structure-propagators
  (rsc-macro-transformer
   (lambda (form defn-env)
     (let* ((type-name (cadr form))
	    (constructor-name (caddr form))
	    (defined-constructor-names
	      (propagator-naming-convention constructor-name))
	    (accessor-names (cdddr form))
	    (accessor-count (length accessor-names)))
       (define (attach-% name)
	 (symbol '% name))
       (define (accessor-definition hidden-name name index)
	 (define (output-reference)
	   (let ((answer (make-vector accessor-count 'nothing)))
	     (vector-set! answer index 'output)
	     (vector->list answer)))
	 `(define-propagator (,hidden-name structure-cell output)
	    (,(car defined-constructor-names)
	     ,@(output-reference)
	     structure-cell)))
       (define (early-access-hack-definition hidden-name name)
	 (let ((expression-variant (cadr (propagator-naming-convention name))))
	   `(define-cell ,expression-variant
	      (early-access-hack
	       ,type-name ,name ,(cadr (propagator-naming-convention hidden-name))))))
       (define (name-fix-definition hidden-name name)
	 `(define-cell ,(car (propagator-naming-convention name))
	    ,(car (propagator-naming-convention hidden-name))))
       `(begin
	  (propagatify ,type-name)
	  (define-by-diagram-variant ,defined-constructor-names
	    (function->cell-carrier-constructor ,constructor-name))
	  ,@(map accessor-definition
		 (map attach-% accessor-names)
		 accessor-names
		 (iota accessor-count))
	  ,@(map name-fix-definition
		 (map attach-% accessor-names)
		 accessor-names)
	  ,@(map early-access-hack-definition
		 (map attach-% accessor-names)
		 accessor-names))))))

(define-structure-propagators pair? cons car cdr)

;;; Here are the old names of these until I sweep them out of the code
(define p:carry-cons  p:cons)
(define e:carry-cons  e:cons)
(define p:carry-pair? p:pair?)
(define e:carry-pair? e:pair?)
(define p:carry-car   p:car)
(define e:carry-car   e:car)
(define p:carry-cdr   p:cdr)
(define e:carry-cdr   e:cdr)

;;; To make lists out of conses, we need empty lists too.
(propagatify null?)
(define p:carry-null? p:null?)
(define e:carry-null? e:null?)

;;; These guys are really the primitive container devices, from which
;;; everything else can be made.
(define-cell p:deposit (function->cell-carrier-constructor identity))
(define-cell e:deposit (expression-style-variant p:deposit))
(define-propagator (p:examine place cell)
  (p:deposit cell place))
(define-cell e:examine
  (early-access-hack cell? identity e:examine))

(define-syntax define-propagator-structure
  (syntax-rules ()
    ((define-propagator-structure arg ...)
     (begin
       (define-structure-propagators arg ...)
       (slotful-information-type arg ...)))))

(define-propagator (c:pair? thing truth)
  (p:pair? thing truth)
  (p:switch truth (cons nothing nothing) thing))

(define-propagator (c:null? thing truth)
  (p:null? thing truth)
  (p:switch truth '() thing))
