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

;;;; Merging

;;; My original thought was that merge answers the question:
;;; 
;;; "What is the least-commitment information structure that captures
;;; all the knowledge in these two information structures?"
;;; 
;;; That was a pretty good place to start, but it turns out not to be
;;; quite adequate.  What's the problem with it, you might ask?  The
;;; problem is that this question cannot have any side-effects.  But
;;; side-effects appear necessary: when merging two TMSes, one must
;;; check the result for consistency, and maybe signal a nogood set if
;;; one discovers a supported contradiction.  Worse, the
;;; carrying-cells strategy for compound data means that you might
;;; have to merge cells, and the only way to do that is to attach
;;; identity propagators between them, which is most definitely an
;;; effect.
;;; 
;;; After long thought, I understand that the real question that a
;;; cell asks (whether or not "merge" is a good name for the function
;;; that computes the answer) is:
;;; 
;;; "What do I need to do to the network in order to make it reflect
;;; the discovery that these two information structures are about the
;;; same object?"
;;; 
;;; In the common case, the answer to this question is going to amount
;;; to just an answer to the previous question, namely "You must
;;; record that that object is best described by this information
;;; structure, which is the least-commitment information structure
;;; that captures all the knowledge in the given information
;;; structures."  (That "you must record" is the set! in add-content).
;;; Also consistent with the simpler idea is the answer "These two
;;; information structures cannot describe the same object."  (This is
;;; the contradictory? test in add-content.)  However, this refined
;;; question provides the opening for more nuanced answers.  For
;;; example, with TMSes, it becomes possible to answer "The object is
;;; described by the following information structure, and you should
;;; record the following nogood set."  Or, with carrying cells, the
;;; answer can be "The object is described by the following
;;; information structure, and you should identify these two cells."
;;; 
;;; The advantage of thinking about it this way is that merge can be a
;;; pure function, which is allowed to return requests for these
;;; effects in addition to refined information structures.  Then places
;;; where merge is called recursively have a chance to intercept and
;;; modify these requests for effects (for example noting that they
;;; must be considered conditional on certain premises), and only 
;;; add-content actually executes the effects that come to it.

;;;; Propagator cells

(define (%make-cell merge)		; message-accepter style
  (let ((neighbors '()) (content nothing)
	(whoiam #f) (history '())
	(probe #f))
    (define (add-content increment informant)
      (let ((info+effects (->effectful (merge content increment))))
        (let ((effects (effectful-effects info+effects))
	      (new-content (effectful-info info+effects)))
	  (if probe (probe))
	  (cond ((eq? new-content content) 'ok)
		((contradictory? new-content)
		 (error "Ack! Inconsistency!"
			(name-stack whoiam) increment)
		 'this-is-not-a-tail-call)
		(else 
		 (set! content new-content)
		 ;; Two debugging aids.
		 (eq-adjoin! content 'visited-cells me)
		 (augment-history! whoiam informant new-content
				   history
				   (lambda (new)
				     (set! history new)))
		 (alert-propagators neighbors)))
	  (for-each execute-effect effects))))
    (define (new-neighbor! new-neighbor)
      (if (not (memq new-neighbor neighbors))
          (begin
            (set! neighbors (cons new-neighbor neighbors))
            (alert-propagators new-neighbor))))
    (define (me message)
      (cond ((eq? message 'content) content)
            ((eq? message 'add-content) add-content)
            ((eq? message 'neighbors) neighbors)
            ((eq? message 'new-neighbor!) new-neighbor!)
	    ((eq? message 'iam!)
	     (lambda (who)
	       (if whoiam (error "Psychotic cell!" who whoiam))
	       (set! whoiam who)))
	    ((eq? message 'who?) whoiam)
	    ((eq? message 'history) history)
	    ;; See ui.scm for probes.
	    ((eq? message 'probe!) (lambda (p) (set! probe p)))
	    ((eq? message 'unprobe!) (set! probe #f))
            (else (error "Unknown message" message))))
    me))

(define (make-cell #!optional merger)
  (define me
    (make-entity
     (lambda (self . args)
       (apply application self args))
     (%make-cell
      (if (default-object? merger)	;Sussman's crock escape hatch. 
	  merge
	  merger))))
  (eq-put! me 'cell #t)
  (((entity-extra me) 'iam!) me)
  (register-diagram me)
  me)

(define (content cell)
  ((entity-extra cell) 'content))
(define (add-content cell increment #!optional informant)
  (((entity-extra cell) 'add-content) increment informant))
(define (neighbors cell)
  ((entity-extra cell) 'neighbors))
(define (new-neighbor! cell neighbor)
  (((entity-extra cell) 'new-neighbor!) neighbor))
(define (who? cell)
  ((entity-extra cell) 'who?))
(define (history cell)
  ((entity-extra cell) 'history))
(define (cell? thing)
  (eq-get thing 'cell))

;;; Default history collector collects the most recent informant only
(define (augment-history! cell informant new-content old-history permission-to-set)
  (permission-to-set `(,informant ,new-content)))


(define (make-named-cell name)
  (name! (make-cell) name))

(define *ensure-cell-generates-constant-propagators* #f)

(define (ensure-cell thing)
  (if (cell? thing)
      thing
      (if *ensure-cell-generates-constant-propagators*
	  ;; TODO Retain forward reference to e:constant?  Copy the code?
	  (let ((answer (e:constant thing)))
	    (add-content answer thing)	; Enables early access
	    answer)
	  (let ((answer (make-named-cell (name thing))))
	    (add-content answer thing)
	    answer))))

;;;; Cellular Generics

(define (merge info1 info2)
  (if (equivalent? info1 info2)
      info1
      (let ((answer (generic-merge info1 info2)))
	(cond ((effectful? answer) answer)
	      ((equivalent? answer info1) info1)
	      ((equivalent? answer info2) info2)
	      (else answer)))))

(define generic-merge
  (make-generic-operator 2 'merge
   (lambda (content increment)
     (if (default-equal? content increment)
         content
         the-contradiction))))

(set-operator-record! merge (get-operator-record generic-merge))

(define (equivalent? info1 info2)
  (or (eqv? info1 info2)
      (generic-equivalent? info1 info2)))

(define generic-equivalent?
  (make-generic-operator 2 'equivalent? default-equal?))

(set-operator-record! equivalent? (get-operator-record generic-equivalent?))

(define the-contradiction #(*the-contradiction*))

(define contradictory?
  (make-generic-operator 1 'contradictory?
   (lambda (thing) (eq? thing the-contradiction))))

(define execute-effect 
  (make-generic-operator 1 'execute-effect (lambda (effect) (effect))))

;;; Merging utilities

(define (implies? v1 v2)
  ;; This is right on the assumption that trivial effects are squeezed
  ;; out (for example by using effectful->).
  (eq? v1 (merge v1 v2)))

;;; This is the n-ary merge
(define (merge* infos-list)
  (fold-left effectful-merge nothing infos-list))

;;; The nothing partial information structure

(define nothing #(*the-nothing*))

(define (nothing? thing)
  (eq? thing nothing))

(defhandler merge
 (lambda (content increment) content)
 any? nothing?)

(defhandler merge
 (lambda (content increment) increment)
 nothing? any?)

;;;; Cells as partial information

(define (equivalent-cells? cell1 cell2)
  (or (eq? cell1 cell2)
      (let ((candidate-bridge-control (eq-get cell1 cell2)))
	(and candidate-bridge-control
	     (equivalent? #t (content candidate-bridge-control))))))

(defhandler equivalent? equivalent-cells? cell? cell?)

(define (cell-merge cell1 cell2)
  (effectful->
   (make-effectful
    cell1
    (list (make-cell-join-effect cell1 cell2 #t)))))

(defhandler merge cell-merge cell? cell?)

;;; Cell joining effects

(define-structure cell-join-effect
  cell1
  cell2
  control)

(define (execute-cell-join effect)
  (let ((cell1 (cell-join-effect-cell1 effect))
	(cell2 (cell-join-effect-cell2 effect))
	(control-info (cell-join-effect-control effect)))
    (let ((control (the-bridge-control cell1 cell2)))
      (add-content control control-info))))

(defhandler execute-effect
  execute-cell-join
  cell-join-effect?)

(define (the-bridge-control cell1 cell2)
  (let ((candidate (eq-get cell1 cell2)))
    (or candidate
	(let ((control (make-named-cell 'bridge-control)))
	  ;; TODO Think about whether this really needs to be
	  ;; symmetric
	  (switch control cell1 cell2)
	  (switch control cell2 cell1)
	  (eq-put! cell1 cell2 control)
	  (eq-put! cell2 cell1 control)
	  control))))

(define (boring-cell-join? effect)
  (let ((cell1 (cell-join-effect-cell1 effect))
	(cell2 (cell-join-effect-cell2 effect))
	(control-info (cell-join-effect-control effect)))
    (or (eq? cell1 cell2)
	(let ((candidate (eq-get cell1 cell2)))
	  (and candidate
	       (implies? (content candidate)
			 control-info))))))

(defhandler redundant-effect? boring-cell-join? cell-join-effect?)

;;; Diagram merging

(defhandler merge merge-diagram %diagram? %diagram?)
(defhandler equivalent? diagram-equivalent? %diagram? %diagram?)

;;; *metadiagram* is the toplevel-diagram for diagram cells.  It is
;;; the only diagram that is not in a cell, and its only purpose is to
;;; hold cells in which diagrams are contained to keep them out of
;;; visualizations of the toplevel-diagram.
(define *metadiagram* (empty-diagram 'metadiagram))

;;; *toplevel-diagram-cell* is the cell containing the
;;; toplevel-diagram.  It belongs to the *metadiagram*
(define *toplevel-diagram-cell*
  (fluid-let ((register-diagram (diagram-inserter *metadiagram*)))
    (make-cell)))
(add-content *toplevel-diagram-cell* *toplevel-diagram*)

;;; Redefine diagram insertion in terms of operations on the
;;; *toplevel-diagram-cell*
(define (diagram-cell-inserter target-diagram-cell)
  (lambda (subdiagram #!optional name)
    ;;; Wrap the subdiagram in a diagram in a cell.
    (let ((subdiagram-wrapper (empty-diagram 'wrapper)))
      (if (default-object? name)
	  (note-diagram-part! subdiagram-wrapper subdiagram)
	  (add-diagram-named-part! subdiagram-wrapper name subdiagram))
      (add-content target-diagram-cell subdiagram-wrapper))
    subdiagram))

(define (register-diagram subdiagram #!optional name)
  ((diagram-cell-inserter *toplevel-diagram-cell*) subdiagram name))

(define (reset-diagrams!)
  ;; Clean out the metadiagram.
  (destroy-diagram! *metadiagram*)
  (set! *metadiagram* (empty-diagram 'metadiagram))
  (fluid-let ((register-diagram (diagram-inserter *metadiagram*)))
    ;; And then, reset the toplevel diagram.
    (set! *toplevel-diagram-cell* (make-cell)))
  ;; Hmmm...  This doesn't look monotonic.
  (destroy-diagram! *toplevel-diagram*)
  (set! *toplevel-diagram* (empty-diagram 'toplevel))
  (set! register-diagram (diagram-cell-inserter *toplevel-diagram-cell*))
  (add-content *toplevel-diagram-cell* *toplevel-diagram*))

(define (empty-diagram-cell identity)
  (let ((diagram-cell
	 (fluid-let ((register-diagram (diagram-inserter *metadiagram*)))
	   (make-cell))))
    (add-content diagram-cell (make-%diagram identity '() '()))
    diagram-cell))

(define (do-make-diagram-for-compound-constructor identity prop-ctor args)
  (with-independent-scheduler
   (lambda ()
     (let ((test-cell-map (map (lambda (arg)
				 (cons (make-cell) arg))
			       args)))
       (fluid-let ((*interesting-cells* (map car test-cell-map)))
	 (apply prop-ctor (map car test-cell-map)))
       ;; The following code shouldn't execute until the diagram
       ;; registrations from prop-ctor are reflected in the
       ;; *toplevel-diagram-cell*
       (propagator *toplevel-diagram-cell*
	 (lambda ()
	   ;; Specifically, we assume that there are parts to the
	   ;; *toplevel-diagram*, so we need to wait until this is
	   ;; true.
	   (if (null? (diagram-parts (contents *toplevel-diagram-cell*)))
	       'ok
	       (let ((prop-ctor-diagram
		      (car
		       ;; There should only be one of these
		       (filter (lambda (x) (not (cell? x)))
			       (map cdr (diagram-parts
					 (contents *toplevel-diagram-cell*)))))))
		 (make-%diagram
		  identity
		  (map (lambda (name.part)
			 (cons (car name.part)
			       (cdr (assq (cdr name.part) test-cell-map))))
		       (filter (lambda (name.part)
				 (assq (cdr name.part) test-cell-map))
			       (diagram-parts prop-ctor-diagram)))
		  (map (lambda (promise)
			 (retarget-promise
			  promise
			  (cdr (assq (diagram-promise-target promise)
				     test-cell-map))))
		       (filter (lambda (promise)
				 (assq (diagram-promise-target promise)
				       test-cell-map))
			       (diagram-promises prop-ctor-diagram))))))))))))
