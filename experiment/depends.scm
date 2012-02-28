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

(define (depends-printer state object)
  (with-current-unparser-state state
    (lambda (port)
      (display "#(depends-on " port)
      (write (v&d-value object) port)
      (display " " port)
      (write (v&d-needs object) port)
      (display " " port)
      (write (v&d-reason object) port)
      (display ")" port))))

(define-structure
 (v&d (named 'depends) (type vector)
      (constructor %make-depends)
      (print-procedure depends-printer)
      (safe-accessors #t))
 value needs reason)


(define (make-depends value needs #!optional reason)
  (%make-depends value needs
     (if (default-object? reason) #f reason)))

(define depends-info v&d-value)
(define depends-premises v&d-needs)
(define depends-premises v&d-reason)
(define depends? v&d?)

(declare-coercion-target depends
			 (lambda (thing)
			   (make-depends thing '() #f)))

(declare-coercion <symbol> ->depends)
(declare-coercion <number> ->depends)
(declare-coercion <boolean> ->depends)
(declare-coercion rtd:%interval ->depends)
(declare-coercion propagator-constructor? ->depends)
(declare-coercion closure? ->depends)
(declare-coercion pair? ->depends)

(define (more-informative-needs? v&d1 v&d2)
  (and (not (lset= eq? (v&d-needs v&d1) (v&d-needs v&d2)))
       (lset<= eq? (v&d-needs v&d1) (v&d-needs v&d2))))

(define (merge-needs . v&ds)
  (apply lset-union eq? (map v&d-needs v&ds)))

(define (v&d-merge v&d1 v&d2)
  (let* ((v&d1-value (v&d-value v&d1))
         (v&d2-value (v&d-value v&d2))
         (value-merge+effects
	  (->effectful (merge v&d1-value v&d2-value))))
    (let ((value-merge
	   (effectful-info value-merge+effects))
	  (value-effects
	   (effectful-effects value-merge+effects)))
      (effectful->
       (make-effectful
	(cond ((eq? value-merge v&d1-value)
	       (if (implies? v&d2-value value-merge)
		   ;; Confirmation of existing information
		   (if (more-informative-needs? v&d2 v&d1)
		       v&d2
		       v&d1)
		   ;; New information is not interesting
		   v&d1))
	      ((eq? value-merge v&d2-value)
	       ;; New information overrides old information
	       v&d2)
	      (else
	       ;; Interesting merge, need both provenances
	       (make-depends value-merge
			     (merge-needs v&d1 v&d2)
			     ????)))
	(map (attach-needs-to-effect (merge-needs v&d1 v&d2))
	     value-effects))))))

(define ((attach-needs-to-effect needs) effect)
  ((generic-attach-premises effect) needs))

(define generic-attach-premises
  (make-generic-operator 1 'attach-needs))

(defhandler generic-attach-premises
  (lambda (effect)
    (lambda (needs)
      (make-cell-join-effect
       (cell-join-effect-cell1 effect)
       (cell-join-effect-cell2 effect)
       (generic-flatten ;; TODO Do I need to do this by flattening?
	(make-tms ;; TODO Get rid of this forward reference
	 (make-depends
	  (cell-join-effect-control effect)
	  needs
	  ????))))))
  cell-join-effect?)

(defhandler-coercing merge v&d-merge ->depends)

(define (v&d-equivalent? v&d1 v&d2)
  (and (lset= eq? (v&d-needs v&d1) (v&d-needs v&d2))
       (equivalent? (v&d-value v&d1) (v&d-value v&d2))))

(defhandler equivalent? v&d-equivalent? v&d? v&d?)

(defhandler contradictory?
 (lambda (v&d) (contradictory? (v&d-value v&d)))
 v&d?)

(define (v&d-> v&d)
  (if (nothing? (v&d-value v&d))
      nothing
      v&d))

(define (v&d-binary-map v&d1 v&d2)
  (lambda (f)
    (v&d->
     (make-depends
      (f (v&d-value v&d1) (v&d-value v&d2))
      (merge-needs v&d1 v&d2)
      ????))))

(defhandler-coercing binary-map v&d-binary-map ->depends)

(defhandler generic-unpack
  (lambda (v&d function)
    (make-depends
     (generic-bind (v&d-value v&d) function)
     (v&d-needs v&d)
     ????))
  v&d? any?)

;;; This particular predicate dispatch system doesn't actually do 
;;; predicate specificity computations.  However, defining the most
;;; general handler first has the desired effect.
(defhandler generic-flatten
  (lambda (v&d) v&d)
  v&d?)

(defhandler generic-flatten
  (lambda (v&d) nothing)
  (lambda (thing)
    (and (v&d? thing)
	 (nothing? (v&d-value thing)))))

(defhandler generic-flatten
  (lambda (v&d)
    (generic-flatten
     (make-depends
      (v&d-value (v&d-value v&d))
      (merge-needs v&d (v&d-value v&d))
      ????)))
  (lambda (thing)
    (and (v&d? thing)
	 (v&d? (v&d-value thing)))))
