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

(define-structure
  (symbolic-metadata (conc-name symbolic-) (safe-accessors #t))
  variable-order
  substitutions
  residual-equations)

(define (empty-metadata)
  (make-symbolic-metadata '() '() '()))

(define (same-metadata? meta1 meta2)
  (and (equal? (symbolic-variable-order meta1)
	       (symbolic-variable-order meta2))
       (equal? (symbolic-substitutions meta1)
	       (symbolic-substitutions meta2))
       (equal? (symbolic-residual-equations meta1)
	       (symbolic-residual-equations meta2))))

(define (combine-variable-orders order1 order2)
  (append
   order1
   (filter (lambda (v)
	     (not (member v order1 same-variable?)))
	   order2)))

(define (same-variable? var1 var2)
  (eq? var1 var2))

(define (unify-metadata meta1 meta2)
  (let ((new-variable-order (combine-variable-orders
			     (symbolic-variable-order meta1)
			     (symbolic-variable-order meta2))))
    (let ((solution
	   (solve-incremental
	    (append
	     (substs->equations (symbolic-substitutions meta2))
	     (symbolic-residual-equations meta1)
	     (symbolic-residual-equations meta2))
	    new-variable-order
	    (symbolic-substitutions meta1))))
      ;; TODO Check the solution for being contradictory
      (make-symbolic-metadata
       new-variable-order
       (substitutions solution)
       (residual-equations solution)))))

(define (list-unify-metadata metadatas)
  (reduce unify-metadata (empty-metadata) metadatas))

(define-structure
  (symbolic (constructor %make-symbolic) (safe-accessors #t))
  expression
  metadata)
(declare-type-tester symbolic? rtd:symbolic)

(declare-coercion rtd:symbolic ->contingent)

(declare-coercion-target symbolic
  (lambda (thing) (make-symbolic thing (empty-metadata))))
(declare-coercion <number> ->symbolic)
(declare-coercion <symbol> ->symbolic)

(define (make-symbolic expression metadata)
  (%make-symbolic
   (simplify (apply-substitutions expression (symbolic-substitutions metadata)))
   metadata))

(define (same-symbolic? symb1 symb2)
  ;; TODO I really want good generic equality!
  (and (symbolic? symb1)
       (symbolic? symb2)
       (equal? (symbolic-expression symb1)
	       (symbolic-expression symb2))
       (same-metadata? (symbolic-metadata symb1)
		       (symbolic-metadata symb2))))

(define (symbolic-merge symb1 symb2)
  (let* ((new-metadata (unify-metadata (symbolic-metadata symb1)
				       (symbolic-metadata symb2)))
	 (expr1 (symbolic-expression symb1))
	 (expr2 (symbolic-expression symb2))
	 (equation (symb:- expr1 expr2))
	 (solution
	  (solve-incremental
	   (cons (make-equation equation '())
		 (symbolic-residual-equations new-metadata))
	   (symbolic-variable-order new-metadata)
	   (symbolic-substitutions new-metadata))))
    (cond ((eqn-contradiction? solution)
	   the-contradiction)
	  (else
	   (make-symbolic
	    expr1
	    (make-symbolic-metadata
	     (symbolic-variable-order new-metadata)
	     (substitutions solution)
	     (residual-equations solution)))))))

(defhandler-coercing merge symbolic-merge ->symbolic)
(defhandler-coercing equivalent? same-symbolic? ->symbolic)

(define (symbolic-binary-map symb1 symb2)
  (lambda (f)
    (let ((answer (f (symbolic-expression symb1) (symbolic-expression symb2))))
      ;; This check protects against confusion engendered by >
      ;; returning nothing or a boolean.  I will need to understand
      ;; more carefully what's going on to do this more cleanly.  In
      ;; particular, doing things this way drops hard-earned metadata
      ;; on the floor.
      (if (or (nothing? answer) (boolean? answer))
	  answer
	  (make-symbolic
	   answer
	   (unify-metadata (symbolic-metadata symb1) (symbolic-metadata symb2)))))))

(defhandler-coercing binary-map symbolic-binary-map ->symbolic)

;;; Two ways to add symbolic expressions as a partial information type.
;;; One way is to use the nary-unpacking machinery:

(defhandler generic-unpack
  (lambda (symbolic function)
    (%make-symbolic ; The simplify in make-symbolic chokes on nothings
     (generic-bind (symbolic-expression symbolic) function)
     (symbolic-metadata symbolic)))
  symbolic? any?)

(defhandler generic-flatten
  (lambda (symbolic)
    (make-symbolic ; Invoke the simplify that didn't happen in generic-unpack
     (symbolic-expression symbolic)
     (symbolic-metadata symbolic)))
  symbolic?)

(defhandler generic-flatten
  (lambda (symb1)
    (generic-flatten
     (make-symbolic
      (generic-flatten (symbolic-expression (symbolic-expression symb1)))
      (unify-metadata (symbolic-metadata symb1)
		      (symbolic-metadata (symbolic-expression symb1))))))
  (guard rtd:symbolic (lambda (thing) (symbolic? (symbolic-expression thing)))))

(defhandler generic-flatten
  (lambda (thing) nothing)
  (guard rtd:symbolic (lambda (thing) (nothing? (symbolic-expression thing)))))

(defhandler generic-flatten
  (lambda (symbolic)
    (let ((the-tms (generic-flatten (symbolic-expression symbolic))))
      (let ((the-value (tms-query the-tms)))
	(if (nothing? the-value)
	    nothing
	    (generic-flatten
	     (make-tms
	      (supported
	       (generic-flatten
		(%make-symbolic
		 (generic-flatten (v&s-value the-value))
		 (symbolic-metadata symbolic)))
	       (v&s-support the-value)
	       (v&s-informants the-value))))))))
  (guard rtd:symbolic (lambda (thing) (tms? (symbolic-expression thing)))))

;;; The other way is the old school, adding methods to every generic
;;; operation:
#;
(define (symbolic-unpacking f)
  (lambda args
    (make-symbolic
     (apply f (map symbolic-expression args))
     (list-unify-metadata (map symbolic-metadata args)))))

#;
(define (coerce-symbolic operator)
  (case (generic-operator-arity operator)
    ((1)
     (defhandler operator (symbolic-unpacking operator) symbolic?))
    ((2)
     (defhandler operator (symbolic-unpacking operator) symbolic? symbolic?)
     (defhandler operator (coercing ->symbolic operator) symbolic? symbolic-able?)
     (defhandler operator (coercing ->symbolic operator) symbolic-able? symbolic?))))
#;
(for-each coerce-symbolic
 (list generic-+ generic-- generic-* generic-/
       generic-= generic-< generic-> generic-<= generic->=
       generic-and generic-or
       generic-abs generic-square generic-sqrt generic-not))

;;; The old school method is annoying because one needs to maintain a
;;; complete list of all the generic operations one might want to
;;; augment, and it doesn't really scale to applications like teaching
;;; car to handle TMSes.  On the other hand, the new school method is
;;; annoying because it's asymmetric in the arguments (there's no good
;;; way to say "if either argument is nothing, don't bother me"), and
;;; because repacked but not yet flattened structures often seem to
;;; violate that data type's invariants.  TMSes and symbolic
;;; expressions are a case in point: they are not written to handle
;;; having a nothing for payload, but generic-unpack shoves it right
;;; in so that generic-flatten can take it back out again.  Maybe this
;;; is why Haskell chose bind as the fundamental monadic operation.

;;; The asymmetry can be largely solved by defining
;;; generic-binary-unpack (which is used inside a binary bind,
;;; followed by the same flatten).

;;; The new school method also has the problem that it isn't really
;;; very good for the comparison operators, because this data type is
;;; really only applicable to numbers, not to boolean values (unless I
;;; change it to be applicable to boolean values too...)

(define (make-variable)
  (generate-uninterned-symbol 'x))

(define (variable->symbolic variable)
  (make-symbolic
   variable
   (make-symbolic-metadata (list variable) '() '())))

(define (plunker cell #!optional variable)
  (let ((my-var (if (default-object? variable)
		    (make-variable)
		    variable)))
    ((constant (variable->symbolic my-var)) cell)))
