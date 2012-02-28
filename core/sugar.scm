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

;;;; Carcinogens for the semicolon part 2: Defining propagators

;;; Here be macros that provide syntactic sugar for playing with the
;;; propagator language as embedded in Scheme.  Syntactic regularities
;;; in patterns of definition of propagator constructors are captured.

;;;; Paired propagator definitions

;;; Propagator objects are usually defined in pairs, one preferring to
;;; be applied diagram-style, and one preferring to be applied
;;; expression-style.  These two macros define such pairs of
;;; propagator objects, with the given names.  Said names are
;;; presumably computed by PROPAGATOR-NAMING-CONVENTION, below

(define-syntax define-by-diagram-variant
  (syntax-rules ()
    ((define-by-diagram-variant (diagram-name expression-name) form)
     (begin
       (define-cell diagram-name form)
       (define-cell expression-name
	 (expression-style-variant diagram-name))))))

(define-syntax define-by-expression-variant
  (syntax-rules ()
    ((define-by-diagram-variant (diagram-name expression-name) form)
     (begin
       (define-cell expression-name form)
       (define-cell diagram-name
	 (diagram-style-variant expression-name))))))

;;;; Propagator naming convention

;;; The naming convention is:
;;;   p:foo   the propagator version of foo
;;;   e:foo   the expression-style variant of p:foo
;;;   c:foo   the constraint-propagator version of foo
;;;   ce:foo  the expression-style variant of c:foo

;;; For convenience, this convention includes constraint-propagator
;;; versions of the various propagators.  The procedure
;;; PROPAGATOR-NAMING-CONVENTION is a macro-helper; it constructs a
;;; pair of names derived from the given name, one to name the
;;; diagram-style variant and one to name the expression-style
;;; variant.  This is calibrated for use with
;;; DEFINE-BY-DIAGRAM-VARIANT and DEFINE-BY-EXPRESSION-VARIANT, above.

(define (propagator-naming-convention name)
  (let* ((name-string (symbol->string name))
	 (long-named? (and (>= (string-length name-string) 3)
			   (equal? "ce:" (substring name-string 0 3))))
	 (propagator-named?
	  (and (>= (string-length name-string) 2)
	       (or (equal? "p:" (substring name-string 0 2))
		   (equal? "e:" (substring name-string 0 2)))))
	 (constraint-named?
	  (and (>= (string-length name-string) 2)
	       (or (equal? "c:" (substring name-string 0 2))
		   long-named?)))
	 (prefix-length
	  (cond (long-named? 3)
		((or constraint-named? propagator-named?) 2)
		(else 0)))
	 (base-name (string-tail name-string prefix-length)))
    (if constraint-named?
	(list (symbol 'c: base-name)
	      (symbol 'ce: base-name))
	(list (symbol 'p: base-name)
	      (symbol 'e: base-name)))))

;;;; Defining primitive propagators

;;; The PROPAGATIFY macro automates the process of defining extensible
;;; propagators whose basic operations are Scheme procedures.

;;; FUNCTION->PROPAGATOR-CONSTRUCTOR turns Scheme procedures into
;;; propagator constructors (that make primitive propagators).  In
;;; principle, that's good enough; but two things can be done to make
;;; the resulting propagator easier to extend to different partial
;;; information structures.  First, a generic operation can be defined
;;; and second, the nary-mapping wrapper from generic-definitions.scm
;;; can be applied.  Finally, to complete the definition, an
;;; expression version of the propagator constructor is usually
;;; defined.  PROPAGATIFY does these things:
;;;   (propagatify +)
;;; is equivalent to
;;;   (define generic-+ (make-generic-operator 2 '+ +))
;;;   (define-cell p:+
;;;     (function->propagator-constructor (nary-mapping generic-+)))
;;;   (define-cell e:+ (expression-style-variant p:+))

;;; Note that the generic machinery needs to know the arity of the
;;; generic operation to define.  PROPAGATIFY will make an educated
;;; guess for what that arity should be, but an explicit second
;;; argument can be supplied to fix the arity.  In addition, if the
;;; second argument is present but is not an arity, PROPAGATIFY will
;;; interpret that as a request not to define the generic procedure at
;;; all.  So
;;;   (propagatify + 'no-generic)
;;; would be equivalent to
;;;   (define-cell p:+ (function->propagator-constructor (nary-mapping +)))
;;;   (define-cell e:+ (expression-style-variant p:+))

;;; Finally, sometimes it is appropriate to propagatify a Scheme
;;; procedure directly, without any provision for extensibility.  The
;;; PROPAGATIFY-RAW macro is helpful for this.
;;;   (propagatify-raw +)
;;; would be equivalent to
;;;   (define-cell p:+ (function->propagator-constructor +))
;;;   (define-cell e:+ (expression-style-variant p:+)
;;; Compare (propagatify + 'no-generic).

(define-syntax propagatify-raw
  (rsc-macro-transformer
   (lambda (form defn-env)
     (let* ((propagatee-name (cadr form)))
       `(define-by-diagram-variant
	  ,(propagator-naming-convention propagatee-name)
	  (function->propagator-constructor
	   (name! ,propagatee-name ',propagatee-name)))))))

(define-syntax propagatify
  (rsc-macro-transformer
   (lambda (form defn-env)
     (let* ((propagatee-name (cadr form))
	    (generic-name (symbol 'generic- propagatee-name)))
       `(begin
	  (define ,generic-name
	    (make-arity-detecting-operator
	     ',propagatee-name ,propagatee-name ,@(cddr form)))
	  (define-by-diagram-variant
	    ,(propagator-naming-convention propagatee-name)
	    (function->propagator-constructor
	     (nary-mapping ,generic-name))))))))

(define (make-arity-detecting-operator
	 name default-operation #!optional arity)
  (if (default-object? arity)
      (set! arity (procedure-arity default-operation)))
  ;; The generic machinery only likes fixed arity operations; assume
  ;; that a fully variadic input operation is really the associative
  ;; version of a binary one, and the binary one will do for
  ;; extensibility.
  (cond ((not (procedure-arity? arity))
	 ;; This allows the user to explictly prevent the construction
	 ;; of the generic operation by specifying a bogus arity for
	 ;; it.
	 default-operation)
	((eqv? (procedure-arity-min arity)
	       (procedure-arity-max arity))
	 (make-generic-operator arity name default-operation))
	((and (or (eqv? 0 (procedure-arity-min arity))
		  (eqv? 1 (procedure-arity-min arity)))
	      (eqv? #f (procedure-arity-max arity)))
	 (make-generic-operator 2 name default-operation))
	(else default-operation)))

;;; This is throwback to days of yore, when I still thought that
;;; monads were a good idea.  This is just like PROPAGATIFY, except
;;; that it wraps the propagatee in NARY-UNPACKING instead of
;;; NARY-MAPPING.
(define-syntax propagatify-monadic
  (sc-macro-transformer
   (lambda (form use-env)
     (let* ((propagatee-name (cadr form))
	    (generic-name (symbol 'generic- propagatee-name))
	    (propagatee (close-syntax propagatee-name use-env)))
       `(begin
	  (define ,generic-name
	    (make-arity-detecting-operator
	     ',propagatee-name ,propagatee ,@(cddr form)))
	  (define-by-diagram-variant
	    ,(propagator-naming-convention propagatee-name)
	    (function->propagator-constructor
	     (nary-unpacking ,generic-name))))))))

;;;; Defining "propagator macros"

;;; Scheme is the macro language of this embedded propagator system.
;;; Therefore defining "propagator macros" is just a matter of
;;; defining Scheme procedures.  Some patterns are common, however, so
;;; merit a little macro support.

;;; DEFINE-PROPAGATOR-SYNTAX is (meant to be) just like define, except
;;; that it wraps the body being defined in a DIAGRAM-STYLE-WITH-DIAGRAM
;;; which is a hook for tagging all cells and propagators created
;;; inside the call with a common identity, which can then be passed
;;; on to the graph drawing tools used to inspect the network.
;;; DEFINE-PROPAGATOR-SYNTAX also assigns the formal parameter names
;;; as names to the incoming arguments.  The latter is most useful in
;;; the regime where all the passed arguments are actually cells (as
;;; opposed to, say, Scheme-lists of cells).

(define-syntax define-propagator-syntax
  (syntax-rules ()
    ((define-propagator-syntax (name arg-form ...) body-form ...)
     (define name
       (named-propagator-syntax (name arg-form ...)
	 body-form ...)))
    ;; N.B. This is the clause that will match dot-notation argument lists
    ((define-propagator-syntax name body-form ...)
     (define name
       (expression-style-with-diagram (empty-diagram-cell 'name)
	 (lambda ()
	   body-form ...))))))

;;; This is the "lambda" to define-propagator-syntax's "define".
(define-syntax named-propagator-syntax
  (syntax-rules ()
    ((named-propagator-syntax (name arg-form ...) body-form ...)
     (propagator-constructor!
      (named-lambda (name arg-form ...)
	(expression-style-with-diagram (empty-diagram-cell 'name)
	 (lambda ()
	   (register-diagram arg-form 'arg-form) ...
	   body-form ...)))))))

;;;; Defining compound propagators

;;; DEFINE-PROPAGATOR is to the propagator language what DEFINE is to
;;; Scheme.  These macros make closures --- see physical-closures.scm.
;;; This one defines propagators in diagram style --- that is, all
;;; boundary cells are explicitly named.

(define-syntax define-propagator
  (rsc-macro-transformer
   (lambda (form defn-env)
     (let ((name (caadr form))
	   (formals (cdadr form))
	   (body (cddr form)))
       `(define-%propagator ,(propagator-naming-convention name)
	  ,formals ,@body)))))

(define-syntax define-d:propagator define-propagator)

(define-syntax define-%propagator
  (syntax-rules ()
    ((define-%propagator names (arg ...)
       body ...)
     (define-by-diagram-variant names
       (name!
	(lambda-d:propagator (arg ...)
	  body ...)
	(car 'names))))))

(define-syntax lambda-d:propagator
  (syntax-rules (import)
    ((lambda-d:propagator (arg ...)
       (import cell ...)
       body ...)
     (make-closure
      (naming-lambda (arg ...)
	body ...)
      (list cell ...)))
    ((lambda-d:propagator (arg ...)
       body ...)
     (lambda-d:propagator (arg ...)
       (import)
       body ...))))

;;; This is a convenience for defining closures (with make-closure)
;;; that track the Scheme names given to the incoming cells.
(define-syntax naming-lambda
  (syntax-rules ()
    ((naming-lambda (arg-form ...) body-form ...)
     (lambda (arg-form ...)
       (register-diagram arg-form 'arg-form) ...
       body-form ...))))

;;; DEFINE-E:PROPAGATOR is just like DEFINE-PROPAGATOR, except that
;;; there is one more implicit boundary cell, which is expected to be
;;; returned by the last form in the body being defined.

(define-syntax define-e:propagator
  (rsc-macro-transformer
   (lambda (form defn-env)
     (let ((name (caadr form))
	   (formals (cdadr form))
	   (body (cddr form)))
       `(define-%e:propagator ,(propagator-naming-convention name)
	  ,formals ,@body)))))

(define-syntax define-%e:propagator
  (syntax-rules ()
    ((define-%e:propagator names (arg ...)
       body ...)
     (define-by-expression-variant names
       (name!
	(lambda-e:propagator (arg ...)
	  body ...)
	(cadr 'names))))))

(define-syntax lambda-e:propagator
  (syntax-rules (import)
    ((lambda-e:propagator (arg ...)
       (import cell ...)
       body ...)
     (make-e:closure
      (naming-lambda (arg ...)
	body ...)
      (list cell ...)))
    ((lambda-e:propagator (arg ...)
       body ...)
     (lambda-e:propagator (arg ...)
       (import)
       body ...))))

;;;     TODO I need variable arity propagator constructors; this can
;;; be taken from the story for compound data.
;;;     TODO Here's an idea: maybe the arguments to the Scheme
;;; procedures produced by define-propagator and company should
;;; be optional.  If any are not supplied, that procedure can just
;;; generate them.  It may also be fun to standardize on a mechanism
;;; like E:INSPECTABLE-OBJECT and THE from the circuits exploration
;;; for reaching in and grabbing such cells from the outside.

;;; TODO Consider rewriting p:when and company in terms of
;;; constructing and applying closures that correspond to the bodies
;;; of the branches.  Then the introduction of switches becomes
;;; automatic, and the possible zero-inputs bug is avoided.
#;
(define-syntax p:when
  (syntax-rules ()
    ((p:when (shieldee ...) conditional body ...)
     (let-cells ((shieldee (e:conditional-wire conditional shieldee)) ...)
       ((delayed-propagator-constructor
	 (lambda (shieldee ...)
	   body ...))
	shieldee ...)))))

(define-syntax p:when
  (syntax-rules ()
    ((p:when (shieldee ...) conditional body ...)
     (application
      (e:conditional-wire conditional
       (make-closure
	(delayed-propagator-constructor
	 (lambda (shieldee ...)
	   body ...))
	(list)))
      shieldee ...))))

(define-syntax p:unless
  (syntax-rules ()
    ((p:unless shieldees conditional stuff ...)
     (p:when shieldees (e:not conditional) stuff ...))))

(define-syntax p:if
  (syntax-rules ()
    ((p:if shieldees conditional consequent alternate)
     (let-cell (conditional-value conditional)
       (p:when shieldees conditional-value consequent)
       (p:unless shieldees conditional-value alternate)))))
#;
(define-syntax e:when
  (syntax-rules ()
    ((e:when (shieldee ...) conditional body ...)
     (let-cells ((shieldee (e:conditional-wire conditional shieldee)) ...)
       (let-cell output
	 ((delayed-propagator-constructor
	   (lambda boundary
	     (handle-explicit-output boundary
	      (lambda (args)
		(apply 
		 (lambda (shieldee ...)
		   body ...)
		 args)))))
	  shieldee ... output)
	 (e:conditional-wire conditional output))))))

(define-syntax e:when
  (syntax-rules ()
    ((e:when (shieldee ...) conditional body ...)
     (e:application
      (e:conditional-wire conditional
       (make-closure
	(delayed-propagator-constructor
	 (lambda boundary
	   (handle-explicit-output boundary
	    (lambda (args)
	      (apply
	       (lambda (shieldee ...)
		 body ...)
	       args)))))
	(list)))
      shieldee ...))))

(define-syntax e:unless
  (syntax-rules ()
    ((e:unless shieldees conditional stuff ...)
     (e:when shieldees (e:not conditional) stuff ...))))

(define-syntax e:if
  (syntax-rules ()
    ((e:if shieldees conditional consequent alternate)
     (let-cell (conditional-value conditional)
       (ce:== (e:when shieldees conditional-value consequent)
	      (e:unless shieldees conditional-value alternate))))))
