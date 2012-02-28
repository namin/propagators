;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul.
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

;;;; Applying the contents of cells

;;; APPLICATION is to the propagator world what APPLY is to Scheme.

;;; Just as putting a Scheme variable into operator position produces
;;; a call to APPLY, putting a cell into operator position produces a
;;; call to APPLICATION.  APPLICATION is a distinguished propagator
;;; constructor that collects a propagator constructor from a cell
;;; and invokes it on argument cells.

;;; The propagator constructors found in cells may either be
;;; primitive, as defined for example by
;;; FUNCTION->PROPAGATOR-CONSTRUCTOR, or may be closures, per the
;;; closure data structure in physical-closures.scm.  That distinction
;;; is the same as the distinction between primitive and compound
;;; Scheme procedures.

;;; The important thing for APPLICATION to deal with, which is new to
;;; the propagator world and is not found in Scheme, is the fact that
;;; the available information about the propagator constructor being
;;; applied may be partial; and that the propagator created by
;;; APPLICATION needs to be properly idempotent, because it may be
;;; called multiple times as that partial information is refined.
;;; This is done by making the transfer of information across the call
;;; boundary conditional on the propagator constructor being applied,
;;; with the effect that both the arguments and the return values
;;; inherit any partialness of that particular propagator constructor.

;;; APPLICATION is the locus of a nontrivial optimization: if the cell
;;; containing the object to be applied is fully determined at network
;;; construction time, the appropriate propagator can be extracted
;;; therefrom and attached immediately, without creating an additional
;;; propagator whose only job would be to pull it out and apply it.

;;; There is also a linguistic matter that APPLICATION needs to deal
;;; with, that doesn't happen in Scheme.  This matter is the
;;; distinction between diagram-style and expression-style propagator
;;; constructors:

;;; The most general propagator notation supplies all the input and
;;; output cells to the desired propagator constructor explicitly:
;;;   (p:+ x y subtotal)
;;;   (p:+ subtotal z total)
;;; This "diagram style" notation is very flexible, because it
;;; allows easy handling of multiple propagators writing to the same
;;; cells, propagators having multiple output cells, having cells that
;;; are ambiguous as to input vs output, etc.

;;; A nested expression notation can be very convenient for simple
;;; cases, however, because it allows the outputs of one propagator to
;;; be piped directly into the inputs to another, without even naming
;;; the intermediate value:
;;;   (e:+ (e:+ x y) z)

;;; APPLICATION comes in the user-callable flavors d@ and e@, which
;;; force diagram-style or expression-style application, respectively.
;;; The native APPLICATION will respect the preferred style of the
;;; propagator being applied if that propagator is completely
;;; determined at network-construction time; otherwise it defaults to
;;; diagram-style.

(define (application object . arg-cells)
  (try-eager-application object
   (lambda (object)
     (if (prefers-diagram-style? object)
	 (eager-diagram-apply object arg-cells)
	 (eager-expression-apply object arg-cells)))
   (lambda (cell)
     (general-propagator-apply cell arg-cells))))

;;; General application

(define (general-propagator-apply prop-cell arg-cells)
  (define done-props '())
  (define (done? prop)
    (member prop done-props equivalent-closures?))
  (define (arg-copier pass?)
    (lambda (arg)
      (let-cell arg-copy
	(conditional-wire pass? (ensure-cell arg) arg-copy)
	arg-copy)))
  ;; This assumes that closures are "carrying cells" compound
  ;; structures rather than "copying data".
  (define (apply-diagram-style prop pass? arg-cells)
    (do-apply-prop prop (map (arg-copier pass?) arg-cells)))
  (define (apply-expression-style prop pass? arg-cells)
    (let ((input-cells (except-last-pair arg-cells))
	  (output-cell (car (last-pair arg-cells))))
      (conditional-wire pass? output-cell
	(ensure-cell
	 (do-apply-prop
	  prop (map (arg-copier pass?) input-cells))))))
  (define (attach prop)
    (set! done-props (cons prop done-props))
    (let-cells (pass? key)
      (add-content key prop)
      (p:equivalent-closures? prop-cell key pass?)
      (if (diagram-style? prop)
	  (apply-diagram-style prop pass? arg-cells)
	  (apply-expression-style prop pass? arg-cells))
      unspecific))
  (let ((the-propagator
	 (lambda ()
	   ((unary-mapping
	     (lambda (prop)
	       (if (done? prop)
		   unspecific
		   (attach prop))))
	    (content prop-cell)))))
    (name! the-propagator 'application)
    (propagator prop-cell the-propagator)
    (register-diagram
     (make-anonymous-i/o-diagram
      the-propagator (list prop-cell) arg-cells))))

;;; Eager application of objects that are fully known at network
;;; construction time.

(define (eager-diagram-apply prop arg-cells)
  (if (diagram-style? prop)
      (do-apply-prop prop arg-cells)
      (handle-explicit-output arg-cells
	(lambda (inputs)
	  (do-apply-prop prop inputs)))))

(define (eager-expression-apply prop arg-cells)
  (if (diagram-style? prop)
      (handle-implicit-cells arg-cells
	(lambda (boundary)
	  (do-apply-prop prop boundary)))
      (if (any implicit-cell? arg-cells)
	  (handle-implicit-cells arg-cells
	    (lambda (boundary)
	      (handle-explicit-output boundary
		(lambda (inputs)
		  (do-apply-prop prop inputs)))))
	  (do-apply-prop prop arg-cells))))

(define (directly-applicable? thing)
  (or (closure? thing)
      (propagator-constructor? thing)))

(define (try-eager-application object direct-apply general-apply)
  (if (cell? object)
      (if (directly-applicable? (content object))
	  (direct-apply (content object))
	  (general-apply object))
      (if (directly-applicable? object)
	  (direct-apply object)
	  (general-apply (ensure-cell object)))))

;;; Massaging boundary shapes

;;; Propagators can be defined either in diagram style (with explicit
;;; cells for their entire boundary) or in expression style (where the
;;; body of the propagator is expected to return one additional cell,
;;; which is in the boundary implicitly).  Propagators can also be
;;; applied in diagram style or in expression style.  So a mismatch
;;; can occur if a propagator is defined one way and applied the other
;;; way.  The procedure HANDLE-EXPLICIT-OUTPUT applies a diagram-style
;;; application to a procedure that expects to be applied in
;;; expression style, and the procedure HANDLE-IMPLICIT-CELLS applies
;;; an expression-style application to a procedure that expects to be
;;; applied in diagram style.  HANDLE-IMPLICIT-CELLS is hairy because
;;; expression-style applications support the %% syntax for selecting
;;; the position of the implicit cell in the supplied argument list,
;;; and because I felt like having it support expression-style
;;; applications that want to return multiple implicit cells.

(define (handle-explicit-output boundary proc)
  (c:== (car (last-pair boundary))
	(proc (except-last-pair boundary))))


(define generate-cell-name
  (let ((cell-counter 0))
    (lambda ()
      (set! cell-counter (+ cell-counter 1))
      (symbol 'cell cell-counter))))
  

(define (handle-implicit-cells inputs proc #!optional num-outputs)
  (if (default-object? num-outputs)
      (set! num-outputs 1))
  (define (manufacture-cell)
    (eq-put! (make-named-cell (generate-cell-name)) 'subexprs inputs))
  (define outputs (map (lambda (k) (manufacture-cell))
		       (iota num-outputs)))
  (define true-inputs
    (let loop ((inputs inputs)
	       (outputs outputs))
      (cond ((null? inputs)
	     outputs)
	    ((implicit-cell? (car inputs))
	     (if (null? outputs)
		 (error "Too many implicit cells" inputs)
		 (cons (car outputs)
		       (loop (cdr inputs) (cdr outputs)))))
	    (else
	     (cons (car inputs) (loop (cdr inputs) outputs))))))
  (proc (map ensure-cell true-inputs))
  (if (= 1 (length outputs))
      (car outputs)
      (apply values outputs)))

(define %% (list 'the-implicit-cell))
(define (implicit-cell? thing)
  (eq? thing %%))
(name! %% '%%)

;;; User-facing frontend for forcing application style

(define (p:application object . arg-cells)
  (try-eager-application object
   (lambda (object)
     (eager-diagram-apply object arg-cells))
   (lambda (cell)
     (general-propagator-apply cell arg-cells))))

(define (functionalize propagator #!optional num-outputs)
  (propagator-constructor!
   (eq-label!
    (lambda inputs
      (handle-implicit-cells inputs
        (lambda (boundary)
	  (apply propagator boundary))
	num-outputs))
    'expression-style #t
    'preferred-style 'expression)))

(define e:application (functionalize p:application))
(define d@ p:application)
(define @d d@)
(define e@ e:application)
(define @e e@)

;;; Guts of applying things

(define (do-apply-prop prop real-args)
  (let ((real-args (map ensure-cell real-args)))
    (cond ((closure? prop)
	   ((if (diagram-style? prop)
		diagram-style-with-diagram
		expression-style-with-diagram)
	    (empty-diagram-cell prop)
	    (lambda ()
	      (apply (closure-code prop) real-args))))
	  ((propagator-constructor? prop)
	   (apply prop real-args))
	  (else (error "Not an applicable propagator" thing)))))

(define (diagram-style-with-diagram target-diagram-cell thunk)
  (let ((explicit-diagram #f)
	(target-diagram-cell
	 (fluid-let ((register-diagram (diagram-inserter *metadiagram*)))
	   (ensure-cell target-diagram-cell))))
    (register-diagram
     (fluid-let
	 ((register-diagram (diagram-cell-inserter target-diagram-cell))
	  (diagram
	   (lambda args
	     (let ((answer (apply make-compound-diagram args)))
	       (set! explicit-diagram answer)
	       answer))))
       (thunk)
       (or explicit-diagram
	   ;; But the content hasn't updated yet!
	   (compute-derived-promises! (content target-diagram-cell)))))))

#|
(define (expression-style-with-diagram target-diagram-cell thunk)
  (let ((target-diagram-cell
	 (let ((register-diagram (diagram-inserter *metadiagram*)))
	   (ensure-cell target-diagram-cell))))
    (fluid-let
	((register-diagram (diagram-cell-inserter target-diagram-cell)))
      (let ((answer (thunk)))
	(register-diagram
	 (compute-derived-promises! (content target-diagram-cell)))
	answer))))
|#

;;; Previous version led to circular structure.

(define (expression-style-with-diagram target-diagram-cell thunk)
  (let ((target-diagram-cell
	 (fluid-let ((register-diagram (diagram-inserter *metadiagram*)))
	   (ensure-cell target-diagram-cell))))
    (let ((answer 
	   (fluid-let
	       ((register-diagram (diagram-cell-inserter target-diagram-cell)))
	     (let ((answer (thunk)))
	       ;; But the content hasn't updated yet!
	       (compute-derived-promises! (content target-diagram-cell))
	       answer))))
      (register-diagram (content target-diagram-cell))
      answer)))


(define (diagram-style? thing)
  (cond ((closure? thing)
	 (closure-diagram-style? thing))
	((propagator-constructor? thing)
	 (not (eq-get thing 'expression-style)))
	(else (error "Propagator style question not applicable" thing))))

;;; Preferred application styles

(define (prefers-diagram-style? thing)
  (let ((preference-tag (eq-get thing 'preferred-style)))
    (cond (preference-tag
	   (not (eq? preference-tag 'expression)))
	  ((closure? thing)
	   (closure-diagram-style? thing))
	  (else #t))))

(define ((tag-preferred-style style) thing)
  (cond ((cell? thing)
	 (let ((answer (make-cell)))
	   (eq-clone! thing answer)
	   (add-content answer ((tag-preferred-style style) (content thing)))
	   answer))
	((propagator-constructor? thing)
	 (let ((answer (lambda args (apply thing args))))
	   (eq-clone! thing answer)
	   (eq-put! answer 'preferred-style style)
	   answer))
	((closure? thing)
	 (eq-put! (closure-copy thing) 'preferred-style style))
	(else 
	 (warn "Ignoring" thing)
	 thing)))

(define (diagram-style-variant thing)
  (ensure-cell ((tag-preferred-style 'diagram) thing)))

(define (expression-style-variant thing)
  (ensure-cell ((tag-preferred-style 'expression) thing)))
