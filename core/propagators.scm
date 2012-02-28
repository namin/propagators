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

;;;; Propagators

;;; A propagator is represented as a Scheme thunk that does that
;;; propagator's job every time the scheduler invokes it.  The thunk
;;; presumably reads the contents of some cells when doing its job;
;;; the system needs to know what those cells are, so that it can wake
;;; the propagator up if the contents of those cells change.  The
;;; thunk also presumably writes to cells (though it can also create
;;; more network structure if needed), but the system doesn't need to
;;; know anything about that.

(define (propagator neighbors to-do)  
  (for-each (lambda (cell)
              (new-neighbor! cell to-do))
            (listify neighbors))
  (eq-put! to-do 'propagator #t)
  (alert-propagator to-do)
  to-do)

(define (propagator? thing)
  (eq-get thing 'propagator))

;;;; Propagator constructors

;;; A propagator constructor is a Scheme procedure that can attach
;;; some network structure to supplied cells.  These are used during
;;; the build portion of the read-build-run propagator execution
;;; model.  To allow for infinite (to wit, dynamically expandable)
;;; networks, run and build can be interleaved.

(define (propagator-constructor? thing)
  (or (eq-get thing 'propagator-constructor)
      ;; TODO This is such a hack!  I probably should not represent
      ;; propagator constructors quite this directly as Scheme
      ;; procedures...
      (and (not (eq-get thing 'not-propagator-constructor))
	   (procedure? thing)
	   (not (cell? thing))
	   (not (propagator? thing))
	   (not (closure? thing)) ; TODO Forward reference :(
	   (warn "Imputing propagator-constructor-hood" thing)
	   #t)))

(define (propagator-constructor! thing)
  (eq-put! thing 'propagator-constructor #t)
  thing)

;;; Returns a propagator constructor that builds single propagators
;;; that execute the supplied Scheme function.
#;
 (define (function->propagator-constructor f)
   (lambda cells
     (let ((output (ensure-cell (car (last-pair cells))))
	   (inputs (map ensure-cell (except-last-pair cells))))
       (propagator inputs                ; The output isn't a neighbor!
	 (lambda ()
	   (add-content output
	     (apply f (map content inputs))))))))

;;; This version has additional metadata to allow the propagator
;;; network to be effectively traversed (see extensions/draw.scm)

(define (function->propagator-constructor f)
  (let ((n (name f)))
    (define (the-constructor . cells)
      (let ((output (ensure-cell (last cells)))
	    (inputs (map ensure-cell (except-last-pair cells)))
	    (the-diagram #f))
	(define (the-propagator)
	  (fluid-let ((*active-diagram* the-diagram))
	    (add-content output
			 (apply f (map content inputs))
			 the-propagator)))
	(name! the-propagator (if (symbol? n)
				  (symbol n ':p)
				  f))
	(propagator inputs the-propagator)
	(set! the-diagram (make-anonymous-i/o-diagram
			   the-propagator inputs (list output)))
	(register-diagram the-diagram)))
    (if (symbol? n) (name! the-constructor (symbol 'p: n)))
    (propagator-constructor! the-constructor)))

;;; Returns a version of the supplied propagator constructor that
;;; creates a propagator that will wait until at least one of the
;;; boundary cells has a non-nothing content and then perform the
;;; indicated construction once.
(define (delayed-propagator-constructor prop-ctor)
  (eq-clone! prop-ctor
   (lambda args
     ;; TODO Can I autodetect "inputs" that should not trigger
     ;; construction?
     (let ((args (map ensure-cell args))
	   (answer-diagram #f))
       (define the-propagator
	 (one-shot-propagator
	  args
	  (lambda ()
	    (fluid-let ((register-diagram
			 (lambda (diagram #!optional name)
			   (replace-diagram! answer-diagram diagram)
			   diagram)))
	      (apply prop-ctor args)))))
       ;; This is the analogue of (compute-aggregate-metadata
       ;; prop-ctor args) TODO much work can be saved by use of the
       ;; diagram made by MAKE-COMPOUND-DIAGRAM.
       (set! answer-diagram
	     (make-diagram-for-compound-constructor
	      the-propagator prop-ctor args))
       (register-diagram answer-diagram)))))

;; This is a peer of PROPAGATOR
(define (one-shot-propagator neighbors action)
  (let ((done? #f) (neighbors (map ensure-cell (listify neighbors))))
    (define (test)
      (if done?
          'ok
          (if (every nothing? (map content neighbors))
              'ok
              (begin (set! done? #t)
		     (action)))))
    (propagator neighbors test)))
