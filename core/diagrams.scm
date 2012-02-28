;;; ----------------------------------------------------------------------
;;; Copyright 2011 Alexey Radul and Gerald Jay Sussman
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
  (%diagram
   safe-accessors
   (constructor %make-%diagram)
   (print-procedure
    (simple-unparser-method '%diagram
     (lambda (object)
       (list (name (diagram-identity object)))))))
  identity
  parts
  promises
  clubs) ; These are the diagrams that have me as a part

;; Cells are also diagrams, with trivial identity, no parts, and no
;; promises.

(define (diagram? thing)
  (or (%diagram? thing)
      (cell? thing)))

(define (diagram-identity thing)
  (if (%diagram? thing)
      (%diagram-identity thing)
      thing))

(define (diagram-parts thing)
  (if (%diagram? thing)
      (%diagram-parts thing)
      (or (eq-get thing 'parts) '())))

(define (set-diagram-parts! thing new-parts)
  (if (%diagram? thing)
      (set-%diagram-parts! thing new-parts)
      (eq-put! thing 'parts new-parts)))

(define (clear-diagram-parts! thing)
  (set-diagram-parts! thing '()))

(define (diagram-promises thing)
  (if (%diagram? thing)
      (%diagram-promises thing)
      (or (eq-get thing 'promises) '())))

(define (set-diagram-promises! thing new-promises)
  (if (%diagram? thing)
      (set-%diagram-promises! thing new-promises)
      (eq-put! thing 'promises new-promises)))

(define (clear-diagram-promises! thing)
  (set-diagram-promises! thing '()))

(define (diagram-clubs thing)
  (if (%diagram? thing)
      (%diagram-clubs thing)
      (or (eq-get thing 'clubs) '())))

(define (set-diagram-clubs! thing new-clubs)
  (if (%diagram? thing)
      (set-%diagram-clubs! thing new-clubs)
      (eq-put! thing 'clubs new-clubs)))

(define (clear-diagram-clubs! thing)
  (set-diagram-clubs! thing '()))

(define (add-diagram-club! thing club)
  (set-diagram-clubs! thing (lset-adjoin eq? (diagram-clubs thing) club)))

(define (remove-diagram-club! thing club)
  ;; delq == lset-delete eq?
  (set-diagram-clubs! thing (delq club (diagram-clubs thing))))

;;; Abstraction barrier

;;; Invariants:
;;; - Every part of a diagram should be a (possibly implicit) diagram.
;;; - Every club a diagram particiaptes in should be a (possibly
;;;   implicit) diagram.
;;; - The clubs list of a diagram X should always contain exactly the
;;;   diagrams that contain X as a part.

(define (make-%diagram identity parts promises)
  (let ((answer (%make-%diagram identity parts promises '())))
    ;; produces (eq-adjoin! output 'shadow-connections the-propagator)
    (for-each (lambda (part)
		(add-diagram-club! part answer))
	      (map cdr parts))
    answer))

(define (empty-diagram identity)
  (make-%diagram identity '() '()))

(define (make-compound-diagram identity parts)
  (make-%diagram identity parts (compute-derived-promises parts)))

(define (compute-derived-promises! diagram)
  (set-diagram-promises!
   diagram
   (lset-union
    diagram-promise-equal?
    (diagram-promises diagram)
    (compute-derived-promises (diagram-parts diagram))))
  diagram)

(define (compute-derived-promises parts)
  ;; For every part that's a cell, I can promise not to read
  ;; (resp. write) it if every part either doesn't mention it or
  ;; promises not to read (resp. write) it.  I just have to take due
  ;; care to make sure that recursive parts are properly taken care
  ;; of.
  (append-map
   (lambda (promised? make-promise)
     (map make-promise
	  (filter
	   (lambda (cell)
	     (every (lambda (part)
		      (recursively-promises? promised? part cell))
		    (map cdr parts)))
	   (filter cell? (map cdr parts)))))
   (list promises-not-to-read? promises-not-to-write?)
   (list promise-not-to-read promise-not-to-write)))

(define diagram make-compound-diagram)

(define (add-diagram-named-part! diagram name part)
  (set-diagram-parts!
   diagram
   (lset-adjoin equal? (diagram-parts diagram) (cons name part)))
  (add-diagram-club! part diagram))

(define (delete-diagram-part! diagram part)
  (set-diagram-parts!
   diagram
   (filter (lambda (name.part)
	     (not (eq? (cdr name.part) part)))
	   (diagram-parts diagram)))
  (remove-diagram-club! part diagram))

(define (names-in-diagram diagram part)
  (map car (filter (lambda (name.part)
		     (eq? part (cdr name.part)))
		   (diagram-parts diagram))))

(define (diagram-creator diagram)
  ;; The creator is the oldest club membership.
  (if (null? (diagram-clubs diagram))
      #f
      (last (diagram-clubs diagram))))

;;;; Implicit diagram production

(define *toplevel-diagram* (empty-diagram 'toplevel))

(define (diagram-inserter target-diagram)
  (lambda (subdiagram #!optional name)
    (if (default-object? name)
	(note-diagram-part! target-diagram subdiagram)
	(add-diagram-named-part! target-diagram name subdiagram))
    subdiagram))

;;; Every propagator constructor is expected to call the procedure
;;; REGISTER-DIAGRAM exactly once on a diagram describing the network
;;; it just constructed.  This procedure is a fluid-bindable hook.  In
;;; addition, a diagram-style propagator constructor is expected to
;;; return that same diagram, whereas an expression-style propagator
;;; constructor is expected to return the cell containing its return
;;; value.

(define (register-diagram subdiagram #!optional name)
  ((diagram-inserter *toplevel-diagram*) subdiagram name))

(define (note-diagram-part! diagram part)
  (if (memq part (map cdr (diagram-parts diagram)))
      'ok
      (add-diagram-named-part! diagram (generate-uninterned-symbol) part)))

(define (delete-diagram-parts! diagram)
  (for-each
   (lambda (part)
     (delete-diagram-part! diagram part)
     (if (null? (diagram-clubs part))
	 (network-unregister part)))
   (map cdr (diagram-parts diagram))))

(define (network-unregister thing)
  (for-each
   (lambda (club)
     (delete-diagram-part! club thing))
   (diagram-clubs thing))
  (delete-diagram-parts! thing))

(define (replace-diagram! diagram new-diagram)
  (delete-diagram-parts! diagram)
  (for-each
   (lambda (name.part)
     (add-diagram-named-part! diagram (car name.part) (cdr name.part)))
   (diagram-parts new-diagram))
  (network-unregister new-diagram))

;;; Getting rid of diagrams when they are no longer needed requires
;;; eliminating appropriate entries in the eq-properties table,
;;; because those values would otherwise point back to themselves.

(define (destroy-diagram! diagram)
  (clear-diagram-clubs! diagram)
  (clear-diagram-promises! diagram)
  (for-each destroy-diagram! (map cdr (diagram-parts diagram)))
  (clear-diagram-parts! diagram))

(define (reset-diagrams!)
  (destroy-diagram! *toplevel-diagram*)
  (set! *toplevel-diagram* (empty-diagram 'toplevel))
  (set! register-diagram (diagram-inserter *toplevel-diagram*)))

;;; Restarting requires resetting the toplevel diagram
(define initialize-scheduler
  (let ((initialize-scheduler initialize-scheduler))
    (lambda ()
      (initialize-scheduler)
      (reset-diagrams!))))

(define with-independent-scheduler
  (let ((with-independent-scheduler with-independent-scheduler))
    (lambda args
      (fluid-let ((*toplevel-diagram* #f)
		  (register-diagram #f))
	(apply with-independent-scheduler args)))))

;;;; New transmitters at the primitive-diagram level

(define-structure diagram-promise
  type
  target)

(define (diagram-promise-equal? prom1 prom2)
  (and (eq? (diagram-promise-type prom1)
	    (diagram-promise-type prom2))
       (eq? (diagram-promise-target prom1)
	    (diagram-promise-target prom2))))

(define (retarget-promise promise new-target)
  (make-diagram-promise (diagram-promise-type promise)
			new-target))

(define (promise-not-to-write thing)
  (make-diagram-promise 'no-write thing))

(define (promise-not-to-read thing)
  (make-diagram-promise 'no-read thing))

(define (make-anonymous-i/o-diagram identity inputs outputs)
  (define (with-synthetic-names lst base)
    (map cons
	 (map symbol (make-list (length lst) base)
	      (iota (length lst)))
	 lst))
  (let* ((parts (append (with-synthetic-names inputs 'input)
			(with-synthetic-names outputs 'output)))
	 (boundary (append inputs outputs))
	 (un-read (lset-difference eq? boundary inputs))
	 (un-written (lset-difference eq? boundary outputs)))
    (make-%diagram
     identity
     parts
     (append (map promise-not-to-write un-written)
	     (map promise-not-to-read un-read)))))

;;; Stuff for automatically determining the i/o characteristics of a
;;; compound box by expanding it out (in a sandbox) and looking at the
;;; i/o characteristics of its structure.

(define *interesting-cells* #f)

(define (make-diagram-for-compound-constructor identity prop-ctor args)
  ;; This check is here to keep recursive compounds from computing
  ;; their internal metadata forever.  The reason this is ok is that
  ;; to learn the metadata of an unexpanded box, I only need to
  ;; observe what propagators want to attach to its interior boundary,
  ;; not to the entire interior.
  (if (or (not *interesting-cells*)
	  (not (null? (lset-intersection eq?
                       *interesting-cells* args))))
      (do-make-diagram-for-compound-constructor identity prop-ctor args)
      (empty-diagram identity)))

(define (do-make-diagram-for-compound-constructor identity prop-ctor args)
  (with-independent-scheduler
   (lambda ()
     (let ((test-cell-map (map (lambda (arg)
				 (cons (make-cell) arg))
			       args)))
       (fluid-let ((*interesting-cells* (map car test-cell-map)))
	 (apply prop-ctor (map car test-cell-map)))
       (let ((prop-ctor-diagram
	      (car
	       ;; There should only be one of these
	       (filter (lambda (x) (not (cell? x)))
		       (map cdr (diagram-parts *toplevel-diagram*))))))
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
		       (diagram-promises prop-ctor-diagram)))))))))

;; Various inspectors should use the diagram-clubs facility instead of
;; the cell neighbors field, which, though somewhat redundant, is used
;; for the scheduler and for a different purpose.

;; Also, all analogues of function->propagator-constructor should be
;; adjusted, and a new one made for compound propagators.

;; ./core/propagators.scm:(define (propagator neighbors to-do)  
;; ./core/propagators.scm:       (propagator inputs                ; The output isn't a neighbor!
;; ./core/propagators.scm:	(propagator inputs the-propagator)))
;; ./core/propagators.scm:    (propagator neighbors test)))
;; ./core/application.scm:    (propagator prop-cell the-propagator)))
;; ./core/search.scm:    (propagator cell amb-choose)))

;; ./extensions/virtual-environments.scm:      (propagator cells
;; ./extensions/virtual-environments.scm:      (propagator cells
;; ./extensions/virtual-closures.scm:  (propagator outside
;; ./extensions/virtual-closures.scm:  (propagator (cons frame-map-cell outside)
;; ./extensions/virtual-closures.scm:  (propagator (list frame-map-cell outside)
;; ./extensions/virtual-closures.scm:  (propagator (list frame-map-cell inside outside)
;; ./extensions/virtual-closures.scm:    (propagator (cons* frame-map-cell closure-cell outside-cells)
;; ./extensions/virtual-closures.scm:  (propagator output

;; ./examples/masyu.scm:  (propagator neighbors
;; ./examples/masyu.scm:  (propagator cells
;; ./examples/masyu.scm:  (propagator (list far-left left right far-right)
;; ./examples/masyu.scm:  (propagator (list far-left left right far-right)
;; ./examples/selectors/selectors.scm:    (propagator inputs the-propagator)))
;; ./examples/selectors/selectors.scm:    (propagator inputs the-propagator)))
;; ./examples/selectors/selectors.scm:    (propagator inputs the-propagator)))

(defhandler name
  (lambda (diagram)
    (let ((own-name (default-name (diagram-identity diagram))))
      (if (not (eq? own-name diagram))
	  own-name
	  (let ((my-names
		 (filter
		  (lambda (x)
		    (not (uninterned-symbol? x)))
		  (append-map
		   (lambda (club)
		     (names-in-diagram club diagram))
		   (diagram-clubs diagram)))))
	    (if (null? my-names)
		diagram
		(last my-names))))))
  diagram?)

(define (promises-not-to-read? diagram part)
  (any (lambda (promise)
	 (and (eq? part (diagram-promise-target promise))
	      (eq? 'no-read (diagram-promise-type promise))))
       (diagram-promises diagram)))

(define (promises-not-to-write? diagram part)
  (any (lambda (promise)
	 (and (eq? part (diagram-promise-target promise))
	      (eq? 'no-write (diagram-promise-type promise))))
       (diagram-promises diagram)))

(define (recursively-promises? direct-promise? diagram part)
  (cond ((direct-promise? diagram part)
	 #t)
	((memq part (map cdr (diagram-parts diagram)))
	 #f)
	(else
	 (every (lambda (subdiagram)
		  (recursively-promises? direct-promise? subdiagram part))
		(map cdr (diagram-parts diagram))))))

;;; The inputs of a diagram X, really, are cells Y that X may read
;;; such that there is a clubs-path from Y to the top that does not go
;;; through X.
(define (internal-to-diagram? diagram subdiagram)
  ;; TODO Avoid losing via loops in the clubs graph
  (cond ((eq? diagram subdiagram)
	 #t)
	((null? (diagram-clubs subdiagram))
	 #f)
	(else
	 (every (lambda (club)
		  (internal-to-diagram? diagram club))
		(diagram-clubs subdiagram)))))

(define (diagram-external-parts diagram)
  (filter (lambda (name.part)
	    (not (internal-to-diagram? diagram (cdr name.part))))
	  (diagram-parts diagram)))

(define (diagram-inputs diagram)
  (filter (lambda (part)
	    (not (promises-not-to-read? diagram part)))
	  (map cdr (diagram-external-parts diagram))))

(define (diagram-outputs diagram)
  (filter (lambda (part)
	    (not (promises-not-to-write? diagram part)))
	  (map cdr (diagram-external-parts diagram))))

(define (diagram-expression-substructure diagram)
  ;; TODO Stub
  (append
   (filter cell? (map cdr (diagram-parts diagram)))
   (filter (lambda (x) (not (cell? x)))
	   (map cdr (diagram-parts diagram)))))

(define (primitive-diagram? diagram)
  (every cell? (map cdr (diagram-parts diagram))))

;;; What does it mean to merge a diagram into another diagram?

;;; The identity (name) of the merged diagram is that of the target
;;;
;;; The parts of the merged diagram is the set-union of the parts of
;;; the target and increment.
;;;
;;; If a promise is present and the same in both the target and
;;; increment, then keep the promise.
;;;
;;; If a promise is present in either the target or the increment but
;;; not both, and the part to which the promise applies is present
;;; only in the diagram in which the promise is made, keep the
;;; promise.

;;; QUESTIONS:
;;;
;;; - Should the existence of the previous diagrams be removed?
;;;   (i.e. should parts replace their membership in the clubs of the
;;;   target and increment with membership in the merge?)
;;;   [CURRENTLY: YES]
;;; - Should it be possible to merge cells? [CURRENTLY: NO]
(define (merge-diagram target increment)
  (let ((merged target))
    ;; Merge parts/clubs
    ;; What if the name is the same?
    (for-each (lambda (part)
		;; The position of the club is significant
		(let ((club.clubs (memq increment (diagram-clubs (cdr part)))))
		  (set-car! club.clubs merged))
		(add-diagram-named-part! merged (car part) (cdr part)))
	      (diagram-parts increment))
    
    ;; Merge promises
    (let ((merged-promises
	   (lset-union
	    ;; Can parts ever be equal? but not eq?
	    (lset-intersection diagram-promise-equal?
			       (diagram-promises target)
			       (diagram-promises increment))
	    (filter (lambda (promise)
		      (not (eq? (memq (diagram-promise-target promise)
				      (map cdr (diagram-parts increment)))
				#f)))
		    (diagram-promises target))
	    (filter (lambda (promise)
		      (not (eq? (memq (diagram-promise-target promise)
				      (map cdr (diagram-parts target)))
				#f)))
		    (diagram-promises increment)))))
      (set-diagram-promises! merged merged-promises))
    
    ;; Finish unregistering the target and increment.
    (clear-diagram-parts! increment)
    (network-unregister increment)
    (clear-diagram-promises! increment)
    
    merged))

(define (diagram-equivalent? target increment)
  (and (= (length (lset-xor diagram-promise-equal?
			    (diagram-promises target)
			    (diagram-promises increment)))
	  0)
       (= (length (lset-xor eq?
			    ;; We just need parts, not names to be the
			    ;; same.
			    (map cdr (diagram-parts target))
			    (map cdr (diagram-parts increment))))
	  0)
       (= (length (lset-xor eq?
			    (diagram-clubs target)
			    (diagram-clubs increment)))
	  0)))
