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

;;; local is a list of known "local" inequalities; applicable to "this
;;; value" only.  These are represented as inequalities in the
;;; designated "variable" 'me.  global is a list of known global
;;; inequalities, which should be transmitted to neighbors as
;;; appropriate.  expression is a symbolic-expression object
;;; describing what is known about the present value with equality.
;;; The local inequalities are only useful if there is no expression
;;; (i.e. the expression is nothing), and will only be present in that
;;; case.

(declare (usual-integrations make-cell cell?))

(define-structure (symb-ineq (safe-accessors #t))
  expression
  local
  global)
(declare-type-tester symb-ineq? rtd:symb-ineq)

(declare-coercion rtd:symb-ineq ->contingent) ;; Really?

(declare-coercion-target symb-ineq
  (lambda (thing) (make-symb-ineq (->symbolic thing) '() '())))
(declare-coercion symbolic? ->symb-ineq)
(declare-coercion symbolic-able? ->symb-ineq)

(define (local->global-inequalities ineq-list)
  (let ((lower-bounds (filter lower-bound-ineq? ineq-list))
	(upper-bounds (filter upper-bound-ineq? ineq-list)))
    (apply
     append
     (map (lambda (lower)
	    (map (lambda (upper)
		   (transitive-ineq lower upper))
		 upper-bounds))
	  lower-bounds))))

(define (apply-substitutions-to-inequality ineq substitutions)
  (%make-inequality
   (inequality-direction ineq)
   (apply-substitutions (inequality-expr1 ineq) substitutions)
   (apply-substitutions (inequality-expr2 ineq) substitutions)))

(define (apply-substitutions-to-inequalities ineqs substitutions)
  (map (lambda (ineq)
	 (apply-substitutions-to-inequality ineq substitutions))
       ineqs))

(define (local+expression->global expression local-ineqs)
  (map normalize-ineq
       (map (lambda (local-ineq)
	      (%make-inequality
	       (inequality-direction local-ineq)
	       expression
	       (inequality-expr2 local-ineq)))
	    local-ineqs)))

(define (symb-ineq-merge symb-ineq1 symb-ineq2)
  (let ((new-expression (generic-flatten
			 (merge (symb-ineq-expression symb-ineq1)
				(symb-ineq-expression symb-ineq2)))))
    (cond ((contradictory? new-expression)
	   the-contradiction)
	  ((nothing? new-expression)
	   (let ((new-local
		  ;; TODO Also simplify them?
		  (delete-duplicates
		   (append (symb-ineq-local symb-ineq1)
			   (symb-ineq-local symb-ineq2)))))
	     (let ((new-global
		    (simplify-inequalities
		     (delete-duplicates
		      (append
		       (local->global-inequalities new-local)
		       (symb-ineq-global symb-ineq1)
		       (symb-ineq-global symb-ineq2))))))
	       (if (not new-global)
		   ;; Solution failed
		   the-contradiction
		   ;; TODO eq?-normalize
		   (make-symb-ineq new-expression new-local new-global)))))
	  ((symbolic? new-expression)
	   (let ((substituted-local
		  (apply-substitutions-to-inequalities
		   (append (symb-ineq-local symb-ineq1)
			   (symb-ineq-local symb-ineq2))
		   (symbolic-substitutions (symbolic-metadata new-expression))))
		 (substituted-global
		  (apply-substitutions-to-inequalities
		   (append (symb-ineq-global symb-ineq1)
			   (symb-ineq-global symb-ineq2))
		   (symbolic-substitutions (symbolic-metadata new-expression)))))
	     (let ((new-global
		    (simplify-inequalities
		     (append
		      (local+expression->global
		       (symbolic-expression new-expression)
		       substituted-local)
		      substituted-global))))
	       (if (not new-global)
		   ;; Solution failed
		   the-contradiction
		   ;; TODO eq?-normalize
		   (make-symb-ineq new-expression '() new-global)))))
	  ((boolean? new-expression)
	   ;; TODO What about keeping track of the hard-earned metadata?
	   new-expression)
	  (else
	   (error "Wah!  symb-ineq-merge doesn't know how to handle" new-expression)))))

(define (same-symb-ineq? symb-ineq1 symb-ineq2)
  (and (symb-ineq? symb-ineq1)
       (symb-ineq? symb-ineq2)
       (or (equal? (symb-ineq-expression symb-ineq1)
		   (symb-ineq-expression symb-ineq2))
	   (same-symbolic? (symb-ineq-expression symb-ineq1)
			   (symb-ineq-expression symb-ineq2)))
       (equal? (symb-ineq-local symb-ineq1)
	       (symb-ineq-local symb-ineq2))
       (equal? (symb-ineq-global symb-ineq1)
	       (symb-ineq-global symb-ineq2))))

(defhandler-coercing merge symb-ineq-merge ->symb-ineq)
(defhandler-coercing equivalent? same-symb-ineq? ->symb-ineq)

(define (symb-ineq-binary-map ineq1 ineq2)
  (lambda (f)
    (merge
     (make-symb-ineq nothing '() (symb-ineq-global ineq1))
     (make-symb-ineq
      (f (symb-ineq-expression ineq1)
	 (symb-ineq-expression ineq2))
      '()
      (symb-ineq-global ineq2)))))

(defhandler-coercing binary-map symb-ineq-binary-map ->symb-ineq)

(defhandler generic-unpack
  (lambda (symb-ineq function)
    (make-symb-ineq
     (generic-bind (symb-ineq-expression symb-ineq) function)
     '()  ;; Never pass on the local stuff
     (symb-ineq-global symb-ineq)))
  symb-ineq? any?)

(defhandler generic-flatten
  (lambda (symb-ineq) symb-ineq)
  symb-ineq?)

;;; TODO This (and its analogue for regular symbolics) is dubious,
;;; because it may throw away metadata that might otherwise be worth
;;; transmitting.  Oh well.
#;
(defhandler generic-flatten
  (lambda (thing) nothing)
  (guard rtd:symb-ineq (lambda (thing) (nothing? (symb-ineq-expression thing)))))

#;
(defhandler generic-flatten
  (lambda (thing) nothing)
  (guard rtd:symb-ineq (lambda (thing) 
			 (nothing? (symb-ineq-expression thing))
			 (null? (symb-ineq-local thing))
			 (null? (symb-ineq-global thing)))))

(defhandler generic-flatten
  (lambda (symb-ineq)
    (generic-flatten
     (let ((sub-ineq (symb-ineq-expression symb-ineq)))
       ;; TODO I wonder whether merge is generally useful for flattening...
       (merge
	(make-symb-ineq
	 nothing
	 (symb-ineq-local symb-ineq)
	 (symb-ineq-global symb-ineq))
	sub-ineq))))
  (guard rtd:symb-ineq (lambda (thing) (symb-ineq? (symb-ineq-expression thing)))))

;;; TODO Why am I writing so many methods for generic-flatten that
;;; just flip things inside out?  Could a binary version of bind work
;;; better?
(defhandler generic-flatten
  (lambda (symbolic)
    (let ((sub-ineq (symbolic-expression symbolic))) 
      (generic-flatten
       (make-symb-ineq
	(generic-flatten
	 (%make-symbolic
	  (generic-flatten (symb-ineq-expression sub-ineq))
	  (symbolic-metadata symbolic)))
	(symb-ineq-local sub-ineq)
	(symb-ineq-global sub-ineq)))))
  (guard rtd:symbolic (lambda (thing) (symb-ineq? (symbolic-expression thing)))))

(defhandler generic-flatten
  (lambda (symb-ineq)
    (let ((the-tms (generic-flatten (symb-ineq-expression symb-ineq))))
      (let ((the-value (tms-query the-tms)))
	(if (nothing? the-value)
	    nothing
	    (generic-flatten
	     (make-tms
	      (supported
	       (generic-flatten 
		(make-symb-ineq
		 (generic-flatten (v&s-value the-value))
		 (symb-ineq-local symb-ineq)
		 (symb-ineq-global symb-ineq)))
	       (v&s-support the-value)
	       (v&s-informants the-value))))))))
  (guard rtd:symb-ineq (lambda (thing) (tms? (symb-ineq-expression thing)))))

(defhandler generic-flatten
  (lambda (symb-ineq)
    (symb-ineq-expression symb-ineq))
  (guard rtd:symb-ineq (lambda (thing) (boolean? (symb-ineq-expression thing)))))

(defhandler generic-flatten
  (lambda (symbolic)
    (symbolic-expression symbolic))
  (guard rtd:symbolic (lambda (thing) (boolean? (symbolic-expression thing)))))

(define ((ineq-enforcer-func direction) in)
  (make-symb-ineq
   nothing
   (list (%make-inequality direction 'me in))
   '()))

(define (ineq-enforcer direction)
  (function->propagator-constructor
   (nary-unpacking
    (ineq-enforcer-func direction))))

(define ((toggled-enforcer direction) control in out)
  (let-cell intermediate
    (p:switch control in intermediate)
    ((ineq-enforcer (reverse-sense direction)) intermediate out)))

(define ((ineq-constraint prop dir) control in out)
  (let-cell not-control
    (p:not control not-control)
    (prop in out control)
    ((toggled-enforcer dir) control in out)
    ((toggled-enforcer (reverse-sense dir)) control out in)
    ((toggled-enforcer (negate-sense dir)) not-control in out)
    ((toggled-enforcer (reverse-sense (negate-sense dir))) not-control out in)))

(define c:>  (ineq-constraint p:>  '>))
(define c:>= (ineq-constraint p:>= '>=))
(define c:<= (ineq-constraint p:<= '<=))
(define c:<  (ineq-constraint p:<  '<))

(define (determined-symbolic? thing)
  (or (and (symbolic? thing)
	   (or (number? (symbolic-expression thing))
	       (boolean? (symbolic-expression thing))
	       (determined-symbolic? (symbolic-expression thing))))
      (and (symb-ineq? thing)
	   (or (number? (symb-ineq-expression thing))
	       (boolean? (symb-ineq-expression thing))
	       (determined-symbolic? (symb-ineq-expression thing))))))

(define (undetermined-symbolic? thing)
  (and (or (symbolic? thing)
	   (symb-ineq? thing))
       (not (determined-symbolic? thing))))

(define (the-determined-answer thing)
  (cond ((not (determined-symbolic? thing))
	 (error "There is no answer" thing))
	((number? thing)
	 thing)
	((boolean? thing)
	 thing)
	((symbolic? thing)
	 (the-determined-answer (symbolic-expression thing)))
	((symb-ineq? thing)
	 (the-determined-answer (symb-ineq-expression thing)))
	(else
	 (error "This shouldn't happen to the-determined-answer" thing))))

(defhandler merge
  (lambda (content increment)
    (merge (the-determined-answer content)
	   (the-determined-answer increment)))
  boolean? determined-symbolic?)

(defhandler merge
  (lambda (content increment)
    (merge (the-determined-answer content)
	   (the-determined-answer increment)))
  determined-symbolic? boolean?)

(defhandler merge
  (lambda (content increment)
    content)
  boolean? undetermined-symbolic?)

(defhandler merge
  (lambda (content increment)
    increment)
  undetermined-symbolic? boolean?)

(define (axch-abstract-number? thing)
  ;; TODO Ugh!
  (or (symbol? thing)
      (and (list? thing)
	   (memq (car thing) '(+ - * /)))))

(define (no-abstract-comparisons operation)
  (defhandler operation
    (lambda (a b) nothing)
    number? axch-abstract-number?)
  (defhandler operation
    (lambda (a b) nothing)
    axch-abstract-number? number?)
  (defhandler operation
    (lambda (a b) nothing)
    axch-abstract-number? axch-abstract-number?))

(no-abstract-comparisons generic->)
(no-abstract-comparisons generic-<)
(no-abstract-comparisons generic->=)
(no-abstract-comparisons generic-<=)

#|
;;; TODO This feels like a hack...
;;; moved to standard-propagators
(defhandler generic-/ 
  (lambda (a b) nothing)
  any? (lambda (x) (and (number? x) (zero? x))))
|#