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

;;;; Fully-virtual environments.  See environments.tex.

(declare (usual-integrations make-cell cell?))

;;;; Frames

;;; A frame tag is a record with an identity and a parent list.  The
;;; notion is that the information in a cell at a frame is the stuff
;;; computed directly in that frame, or anywhere up the chain of
;;; ancestors.  This is orthogonal to whether a frame can have
;;; multiple parents.
(define-structure
  (frame (constructor %make-frame) (safe-accessors #t))
  parents
  strict-ancestors)			; Cache the ancestor computation

(define (%compute-ancestors frames)
  (delete-duplicates (append-map frame-ancestors frames)))

(define (frame-ancestors frame)
  (cons frame (frame-strict-ancestors frame)))

(define (make-frame parents)
  (%make-frame parents (%compute-ancestors parents)))

;;;; Virtual Copy Sets

;;; A virtual copies set is a structure that associates frame tags
;;; (which for the nonce need only be assumed to be eq?-comparable)
;;; with information.
(define-structure
  (virtual-copies (safe-accessors #t))
  alist)
(declare-type-tester virtual-copies? rtd:virtual-copies)

(define alist->virtual-copies make-virtual-copies)
(define virtual-copies->alist virtual-copies-alist)

(define-method generic-match ((pattern <pair>) (object rtd:virtual-copies))
  (generic-match pattern (virtual-copies->alist object)))

(define (frame-binding copy-set frame)
  ;; TODO Of course, an alist is the worst possible data structure for
  ;; this purpose, but it's built-in and it's persistent.
  (assq frame (virtual-copies-alist copy-set)))

(define (occurring-frames copy-set)
  (map car (virtual-copies-alist copy-set)))

(define (occurring-frames* copy-sets)
  (delete-duplicates (append-map occurring-frames copy-sets)))

(define (frame-occurs? copy-set frame)
  (not (not (frame-binding copy-set frame))))

(define (direct-frame-content copy-set frame)
  (let ((occurrence (frame-binding copy-set frame)))
    (if occurrence
	(cdr occurrence)
	nothing)))

;;;; Frame & Copy-set Interactions

;;; The intention is that the full information content of a cell in a
;;; frame is the merge of all information available in that frame and
;;; all that frame's ancestors.  I can implement that intention
;;; directly per below; or I can use one-cell cross-frame propagators
;;; to maintain the invariant that the direct content in every frame
;;; stabilizes to be the same as the intended full content; or I can
;;; hatch some scheme whereby that intention is maintained in some
;;; implicit manner but not represented explicitly.  That's a choice.
(define (full-frame-content copy-set frame)
  (fold merge nothing
	(map (lambda (frame)
	       (direct-frame-content copy-set frame))
	     (frame-ancestors frame))))

(define (ancestral-occurrence-count copy-set frame)
  (count (lambda (frame)
	   (frame-occurs? copy-set frame))
	 (frame-ancestors frame)))

;; See environments.tex for the meaning of "acceptable".
(define (acceptable-frame? frame copy-sets)
  (apply boolean/and
   (map (lambda (copy-set)
	  (<= 1 (ancestral-occurrence-count copy-set frame)))
	copy-sets)))

;; See environments.tex for the meaning of "good".
(define (good-frame? frame copy-sets)
  (and (acceptable-frame? frame copy-sets)
       (not (apply boolean/or
	     (map (lambda (parent)
		    (acceptable-frame? parent copy-sets))
		  (frame-parents frame))))))

(define (good-frames copy-sets)
  ;; TODO I'm *certain* there's a more efficient way to do this
  (filter (lambda (frame)
	    (good-frame? frame copy-sets))
	  (occurring-frames* copy-sets)))

(define (lexical-invariant? copy-set)
  (apply boolean/and
   (map (lambda (frame)
	  (<= (ancestral-occurrence-count copy-set frame) 1))
	(occurring-frames copy-set))))

;; This operation, as named, depends on the lexical invariant above
;; holding good.
(define (the-occurring-parent frame copy-set)
  (find (lambda (parent)
	  (frame-occurs? copy-set parent))
	(frame-ancestors frame)))

;;;; Equating and merging virtual copy sets

(define (v-c-equal? copy-set1 copy-set2)
  (let ((the-frames (occurring-frames copy-set1)))
    (and (lset= eq? the-frames (occurring-frames copy-set2))
	 (apply boolean/and
		(map (lambda (frame)
		       (equivalent? (full-frame-content copy-set1 frame)
				    (full-frame-content copy-set2 frame)))
		     the-frames)))))

;;; This merge is OK if "normal" propagators use v-c-i/o-unpacking
;;; (below) for their operations.  Then they will respect the
;;; occurrence structure so the merge operation doesn't have to.
(define (virtual-copy-merge copy-set1 copy-set2)
  (define (frame-by-frame f)
    (lambda args
      (alist->virtual-copies
       (map (lambda (frame)
	      (cons frame (apply f (map (lambda (arg)
					  (full-frame-content arg frame))
					args))))
	    (occurring-frames* args)))))
  ((frame-by-frame merge) copy-set1 copy-set2))

(defhandler merge virtual-copy-merge virtual-copies? virtual-copies?)
(defhandler equivalent? v-c-equal? virtual-copies? virtual-copies?)

(defhandler contradictory?
  (lambda (vcs)
    (any contradictory? (map cdr (virtual-copies->alist vcs))))
  virtual-copies?)

;;;; Propagator Machinery

;;; Doing virtual copies via the generic-unpack mechanism presents
;;; three problems.  First, imagine a binary operation with two
;;; virtual-copies arguments.  A direct implementation of
;;; virtual-copy-bind would evaluate that operation on all
;;; quadratically many combinations of pairs of frames, and then do
;;; something to only keep the pieces we had wanted.  That could get
;;; ugly.  Second, the unpacking mechanism below actually needs to
;;; look at all the neighbor cells in order to decide which sets of
;;; frames to operate on.  Third, if one goes through the standard
;;; unpack-flatten mechanism, then a binary operation working on a
;;; pair of virtual copies of TMSes of something will find itself
;;; trying to flatten a set of virtual copies of TMSes of virtual
;;; copies of TMSes of something.  Doing that correctly requires a
;;; mechanism to turn a TMS of virtual copies of X into a virtual
;;; copies of a TMS of X; but under the current regime (i.e. without
;;; knowing what type the final result is supposed to be) the
;;; existence of that mechanism will force all TMSes of virtual copies
;;; to become virtual copies of TMSes.  But what if I *want* to
;;; subject the frames to TMS premises in some region of the network?
;;; This is a very general problem.  Are monad transformers such
;;; conversion mechanisms?  Or do they prevent this issue from arising
;;; by some other means?  (Or are they completely unrelated?)  This
;;; whole mess is perhaps a function of not being able to look at what
;;; the client wants.
(define (v-c-i/o-unpacking f)
  (lambda args
    (let ((output (car (last-pair args)))
	  (inputs (except-last-pair args)))
      (alist->virtual-copies
       (map (lambda (frame)
	      (cons (the-occurring-parent frame output)
		    (apply f (map (lambda (copy-set)
				    (full-frame-content copy-set frame))
				  inputs))))
	    (good-frames args))))))

(define (i/o-function->propagator-constructor f)
  (lambda cells
    (let ((output (car (last-pair cells))))
      (propagator cells
	(lambda ()
	  (add-content output (apply f (map content cells))))))))

;; Now the version with the metadata
(define (i/o-function->propagator-constructor f)
  (lambda cells
    (let ((output (car (last-pair cells))))
      (propagator cells
	(eq-label!
	 (lambda ()
	   (add-content output (apply f (map content cells))))
	 ;; TODO Currently ok, because the last "input" is only used
	 ;; for virtualization
	 'inputs (except-last-pair cells)
	 'name f
	 'outputs (list output))))))

(define (doit f)
  (i/o-function->propagator-constructor
   (eq-put!
    (lambda args
      ;; TODO Generalize this to other information types
      (if (any nothing? args)
	  nothing
	  (apply (v-c-i/o-unpacking (nary-unpacking f)) args)))
    'name f)))

;;;; Propagators

(define vc:adder (doit generic-+))
(define vc:subtractor (doit generic--))
(define vc:multiplier (doit generic-*))
(define vc:divider (doit generic-/))

(define vc:absolute-value (doit generic-abs))
(define vc:squarer (doit generic-square))
(define vc:sqrter (doit generic-sqrt))
(define vc:=? (doit generic-=))
(define vc:<? (doit generic-<))
(define vc:>? (doit generic->))
(define vc:<=? (doit generic-<=))
(define vc:>=? (doit generic->=))

(define vc:inverter (doit generic-not))
(define vc:conjoiner (doit generic-and))
(define vc:disjoiner (doit generic-or))

(define (vc:const value)
  (doit (eq-put! (lambda () value) 'name (list 'const value))))
(define vc:switch (doit switch-function))

(define generic-quotient (make-generic-operator 2 'quotient quotient))
(eq-put! generic-quotient 'name 'quotient)
(define vc:quotient (doit generic-quotient))
(define generic-remainder (make-generic-operator 2 'remainder remainder))
(eq-put! generic-remainder 'name 'remainder)
(define vc:remainder (doit generic-remainder))
