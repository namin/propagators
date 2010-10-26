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

;;;; Closures, physical-copies style

;;; A normal propagator constructor in the physical copies style is a
;;; Scheme procedure that, when given some cells, will build some
;;; quantity of network structure onto those cells.  As stated, these
;;; are expected not to be closed (in Scheme) over anything
;;; interesting.

;;; A closure in the physical copies style is a propagator constructor
;;; that may be closed over some cells, together with an explicit list
;;; of those cells.  The list needs to be explicit because in order to
;;; merge closures, I have to merge the cells they are closed over.
;;; (Cell merging is such that the underlying Scheme closures that
;;; implement the propagator construction do not need to be modified
;;; when this happens).

;;; Requiring physical-copies closures to close only over cells
;;; amounts to specifying the "carrying cells" strategy for compound
;;; data, at least with regard to closures.  This feels like the right
;;; thing; but in principle there is no reason to insist on it.  To do
;;; "copying data", MAKE-CLOSURE would need to construct a propagator
;;; that would rebuild the closure every time any of the cells the
;;; environment grabs experienced any changes, and APPLICATION, below,
;;; would need to be adjusted accordingly (how, exactly?)  All this
;;; would be perfectly plausible, with the same pros and cons as the
;;; regular "carrying" vs "copying" debate.  Note that the actual
;;; closure data structure, except for MAKE-CLOSURE, is completely
;;; independent of the carrying vs copying choice, just like the
;;; actual partial information type definition for CONS.

;;; The code-tag field is a hack to let me detect "equality" between
;;; two Scheme closures that have the same code but are closed over
;;; different cells.  Such are the moral equivalent of identical data
;;; structures with different contents, and so are mergeable; whereas
;;; Scheme closures with different code are like data structures of
;;; different types and so are not mergeable.

(define-structure
  (closure (constructor %make-closure) (safe-accessors #t))
  code
  environment
  diagram-style?)

(define (closure-code-tag thing)
  (procedure-lambda (closure-code thing)))

(define (closure-copy closure)
  (eq-clone! closure
   (%make-closure (closure-code closure)
		  (closure-environment closure)
		  (closure-diagram-style? closure))))

;; The ensure-cell here makes these be "carrying cells" structures.
(define (make-closure code environment)
  (name-closure!
   (%make-closure code (map ensure-cell environment) #t)))

(define (make-e:closure code environment)
  (name-closure!
   (%make-closure code (map ensure-cell environment) #f)))

(define (name-closure! closure)
  (cond ((eq-get closure 'name) closure) ; ok
	((eq-get (closure-code closure) 'name)
	 (name! closure (closure-code closure)))
	((symbol? (closure-code-tag closure))
	 (name! closure (closure-code-tag closure)))
	(else ; nothing works
	 closure)))

(define (same-code? closure1 closure2)
  (and (eq? (closure-code-tag closure1) (closure-code-tag closure2))
       (eqv? (closure-diagram-style? closure1)
	     (closure-diagram-style? closure2))))

(define (closure-merge closure1 closure2)
  (if (not (same-code? closure1 closure2))
      the-contradiction
      (effectful-bind (merge (closure-environment closure1)
			     (closure-environment closure2))
	(lambda (new-env)
	  (%make-closure
	   (closure-code closure1)
	   new-env
	   (closure-diagram-style? closure1))))))

(define (equivalent-closures? closure1 closure2)
  (or (eqv? closure1 closure2)
      (and (closure? closure1)
	   (closure? closure2)
	   (eq? (closure-code-tag closure1) (closure-code-tag closure2))
	   (equivalent? (closure-environment closure1)
			(closure-environment closure2)))))

(define (contradictory-closure? closure)
  (contradictory? (closure-environment closure)))

(defhandler merge closure-merge closure? closure?)
(defhandler equivalent? equivalent-closures? closure? closure?)
(defhandler contradictory? contradictory-closure? closure?)

(initialize-scheduler)			; propagatify makes cells!

(propagatify equivalent-closures?)
