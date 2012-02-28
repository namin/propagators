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

(declare (usual-integrations make-cell cell?))

(define (fail-all cells)
  (process-one-contradiction
   (apply append (map v&s-support (filter v&s? (map tms-query (filter tms? (map content cells))))))))

(define (for-each-consistent-state proc cells)
  (set! cells (listify cells))
  (let loop ((last-run-result (run)))
    (if (eq? 'done last-run-result)
	(begin
	  (proc)
	  (fail-all cells)
	  (loop (run))))))

(define map-consistent-states (walker->mapper for-each-consistent-state))

(define-method generic-match ((pattern <vector>) (object rtd:symbolic-metadata))
  (generic-match
   pattern (vector 'metadata (symbolic-variable-order object)
		   (symbolic-substitutions object)
		   (symbolic-residual-equations object))))

(define-method generic-match ((pattern <vector>) (object rtd:symbolic))
  (generic-match
   pattern (vector 'symbolic (symbolic-expression object)
		   (symbolic-metadata object))))

(define-method generic-match ((pattern <vector>) (object rtd:symb-ineq))
  (generic-match
   pattern (vector 'symb-ineq (symb-ineq-expression object)
		   (symb-ineq-local object)
		   (symb-ineq-global object))))

(define-method generic-match ((pattern <pair>) (object rtd:inequality))
  (generic-match pattern `(,@(inequality->list object) ,(inequality-variables object))))

(define-method generic-match ((pattern rtd:inequality) (object rtd:inequality))
  (generic-match (inequality->list pattern) (inequality->list object)))

(define-method generic-match ((pattern <vector>) (object rtd:frs))
  (if (stale-frs? object)
      (generic-match
       pattern (vector 'stale-frs (frs-value object)
		       (frs-support object)))
      (generic-match
       pattern (vector 'frs (frs-value object)
		       (frs-support object)))))

(define-method generic-match ((pattern <vector>) (object rtd:frpremise))
  (generic-match
   pattern (vector 'frp (frpremise-identity object)
		   (frpremise-timestamp object))))

#|
;;; Trying to abstract the above.

 (define (record-type-summarizer record-type-descriptor)
   (lambda (object)
     (list->vector
      (cons (symbol (record-type-name record-type-descriptor))
	    (map (lambda (field-name)
		   ((record-accessor record-type-descriptor field-name)
		    object))
		 (record-type-field-names record-type-descriptor))))))

 (define (declare-match-vector-patterns record-type-descriptor)
   (add-method generic-match
     (make-method (list <vector> record-type-descriptor)
       (lambda (pattern object)
	 (generic-match
	  pattern
	  ((record-type-summarizer record-type-descriptor)
	   object))))))

 (declare-match-vector-patterns rtd:symbolic-metadata)
 (declare-match-vector-patterns rtd:symbolic)
 (declare-match-vector-patterns rtd:symb-ineq)
 (declare-match-vector-patterns rtd:frpremise)
|#
