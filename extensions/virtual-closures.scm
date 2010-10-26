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

;;;; Closures for fully-virtual environments.  See environments.scm
;;;; and environments.tex.

(declare (usual-integrations make-cell cell?))

;;;; Frame Maps

(define-structure
  frame-map
  alist
  default-parents)
(declare-type-tester frame-map? rtd:frame-map)

(define (frame-map-bind frame-map key1 key2)
  (make-frame-map
   (cons (cons key1 key2)
	 (frame-map-alist frame-map))
   (frame-map-default-parents frame-map)))

(define (frame-map-lookup frame-map key)
  (assq key (frame-map-alist frame-map)))

(define (frame-map-get frame-map key)
  (let ((binding (frame-map-lookup frame-map key)))
    (if binding
	(cdr binding)
	(error "Key not found" key frame-map))))

(define (mapping-available? frame-map key)
  (not (not (frame-map-lookup frame-map key))))

;;;; Equating and merging frame maps

(define (same-binding? bind1 bind2)
  (and (eq? (car bind1) (car bind2))
       (eq? (cdr bind1) (cdr bind2))))

(define (frame-map-merge map1 map2)
  (define (repeated-elt? lst)
    (not (lset= eq? lst (delete-duplicates lst))))
  (define (merge-bindings alist1 alist2)
    (let ((answer (lset-union same-binding? alist1 alist2)))
      (if (repeated-elt? (map car answer))
	  (error "Same frame mapped to different targets" map1 map2)
	  answer)))
  (make-frame-map
   (merge-bindings (frame-map-alist map1) (frame-map-alist map2))
   (delete-duplicates (append (frame-map-default-parents map1)
			      (frame-map-default-parents map2)))))

(define (f-m-equal? map1 map2)
  (and (lset= same-binding?
	      (frame-map-alist map1) (frame-map-alist map2))
       (lset= eq? (frame-map-default-parents map1)
	          (frame-map-default-parents map2))))

(defhandler merge frame-map-merge frame-map? frame-map?)
(defhandler equivalent? f-m-equal? frame-map? frame-map?)

;;;; Extending frame maps

(define (complete-frame-map copy-set frame-map)
  (let loop ((frame-map frame-map)
	     (frames (occurring-frames copy-set)))
    (if (null? frames)
	frame-map
	(ensure-mapping-available-for
	 frame-map (car frames)
	 (lambda (new-map) (loop new-map (cdr frames)))))))

(define (ensure-mapping-available-for frame-map key cont)
  (define (doit frame-map)
    (if (mapping-available? frame-map key)
	(cont frame-map)
	(cont (frame-map-bind
	       frame-map key (make-image-for frame-map key)))))
  (let loop ((frame-map frame-map)
	     (parents (frame-parents key)))
    (if (null? parents)
	(doit frame-map)
	(ensure-mapping-available-for
	 frame-map (car parents)
	 (lambda (new-map) (loop new-map (cdr parents)))))))

(define (make-image-for frame-map key)
  (make-frame
   (append
    ;; Do I really want the images of the parents of the current frame
    ;; to be parents of the image?  What if those parents do not have
    ;; images?  should I always create them?  Are there frames that
    ;; should not get images in called abstractions?  Or should not
    ;; always get fresh images?
    ;; Hm.  It seems that it's ok to make lots of images, provided I
    ;; am careful not to generate occurrences in the callee that
    ;; do not correspond to occurrences that exist in the caller.
    ;; On the other hand, perfect consistency with the "extra identity
    ;; propagators" interpretation of physical copies would seem to
    ;; suggest that I only create one image, and copy down from
    ;; parents (and back up, in the case of outputs).
    (map (lambda (parent)
	   (frame-map-get frame-map parent))
	 (frame-parents key))
    ;; If I could promise that all frames eventually topped out at
    ;; frames without incoming parents, then I could choose to attach
    ;; these only to such frames, instead of to all of them.
    (frame-map-default-parents frame-map))))

(define (simple-ensure-mapping frame-map key #!optional parents)
  (if (default-object? parents)
      (set! parents (frame-map-default-parents frame-map)))
  (if (mapping-available? frame-map key)
      frame-map
      (frame-map-bind frame-map key (make-frame parents))))

;;;; Call Sites

(define-structure
  (v-closure (safe-accessors #t))
  inside
  interior
  default-parents)
(declare-type-tester v-closure? rtd:v-closure)

;; This still requires the closure to be known statically.
(define (static-call-site v-closure outside-cells)
  (let ((inside-cells (v-closure-inside v-closure))
	(interior-cells (v-closure-interior v-closure)))
    (if (not (= (length outside-cells)
		(length inside-cells)))
	(error "Differing boundary lengths" outside-cells inside-cells))
    (let ((frame-map (make-frame-map '() (v-closure-default-parents v-closure)))
	  (frame-map-cell (make-named-cell 'frame-map)))
      (add-content frame-map-cell frame-map)
      (interior-copier frame-map-cell outside-cells interior-cells)
      (map (lambda (outside-cell inside-cell)
	     ;; It's not clear whether independent frame mappers are
	     ;; the right thing.  The "extra identity propagators"
	     ;; interpretation of physical copies would, I think,
	     ;; rather suggest that there be one map-keeper for the
	     ;; entire outside, and it ensure that good frames in the
	     ;; outside have images in the inside.  On the other hand,
	     ;; perhaps this is ok, because the interior copier is the
	     ;; one that's actually responsible for creating those
	     ;; occurrences on the inside of the abstraction
	     ;; (including the boundary), and as long as it does the
	     ;; right thing, extra bindings in the frame map don't
	     ;; hurt anyone.  But then, the inward transfer (and
	     ;; outward transfer) would also need to be done on a
	     ;; whole-boundary basis?
	     (map-keeper outside-cell frame-map-cell)
	     (inward-transferrer frame-map-cell inside-cell outside-cell)
	     (outward-transferrer frame-map-cell inside-cell outside-cell))
	   outside-cells
	   inside-cells))))

(define (map-keeper outside frame-map-cell)
  (propagator outside
    (eq-label!
     (lambda ()
       (add-content frame-map-cell
         (complete-frame-map (content outside) (content frame-map-cell))))
     'name 'map-keeper
     'inputs (list outside)
     'outputs (list frame-map-cell))))

(define (interior-copier frame-map-cell outside interior)
  (propagator (cons frame-map-cell outside)
    (eq-label!
     (lambda ()
       (let ((answer (needed-interior-copies (content frame-map-cell)
					     (map content outside))))
	 (for-each (lambda (cell)
		     (add-content cell answer))
		   interior)))
     'name 'interior-copier
     'inputs (cons frame-map-cell outside)
     ;; I'm scared to write the outputs of this one, because it hits
     ;; the whole interior (and only with virtualization information)
     )))

(define (inward-transferrer frame-map-cell inside outside)
  (propagator (list frame-map-cell outside)
    (eq-label!
     (lambda ()
       (add-content inside
         (transfer-inward (content frame-map-cell) (content outside))))
     'name 'inward
     'inputs (list frame-map-cell outside)
     'outputs (list inside))))

(define (outward-transferrer frame-map-cell inside outside)
  (propagator (list frame-map-cell inside outside)
    (eq-label!
     (lambda ()
       (add-content outside
	 (transfer-outward
	  (content frame-map-cell) (content inside) (content outside))))
     'name 'outward
     'inputs (list frame-map-cell inside)
     'outputs (list outside))))

;;;; Actually migrating things across abstraction boundaries

;;; This treats the outside as the boss: new frames that appear on the
;;; outside generate new frame map entries, but new frames that appear
;;; on the inside do not.  Instead, the channels from the inside to
;;; the outside only transport things for which there already are
;;; frame map entries.  Perhaps this has something to do with the fact
;;; that the inside is by design shared by all call sites of a given
;;; abstraction, but each call site has a different outside.

(define (select-submap frame-map frames)
  (let* ((mapped-frames
	  (filter (lambda (frame)
		    (mapping-available? frame-map frame))
		  frames)))
    (map (lambda (frame)
	   (cons frame (frame-map-get frame-map frame)))
	 mapped-frames)))

(define (transfer-inward frame-map outside-copies)
  (if (or (nothing? frame-map) (nothing? outside-copies))
      nothing
      ;; This is careful about mappings
      (alist->virtual-copies
       (map (lambda (frame-pair)
	      (cons (cdr frame-pair)
		    ;; This can be direct-frame-content if all the
		    ;; parents of the inside frame are images of the
		    ;; frame map; but they may not be if stuff is
		    ;; known about the interior abstraction
		    ;; independently of its call sites.
		    ;; TODO In fact, this is the place where things that
		    ;; care about crossing abstraction boundaries might
		    ;; be checked.
		    (full-frame-content outside-copies (car frame-pair))))
	    (select-submap frame-map (occurring-frames outside-copies))))))

(define (transfer-outward frame-map inside-copies outside-copies)
  (if (or (nothing? frame-map)
	  (nothing? inside-copies) (nothing? outside-copies))
      nothing
      ;; This is very careful about occurrences
      (let* ((exterior-bindings
	      (select-submap frame-map (occurring-frames outside-copies)))
	     (bindings
	      (filter
	       (lambda (frame-pair)
		 (frame-occurs? inside-copies (cdr frame-pair)))
	       exterior-bindings)))
	(alist->virtual-copies
	 (map (lambda (frame-pair)
		(cons (car frame-pair)
		      ;; direct- vs full- here as above
		      (full-frame-content inside-copies (cdr frame-pair))))
	      bindings)))))

(define (needed-interior-copies frame-map outside-vcs)
  (if (or (nothing? frame-map) (any nothing? outside-vcs))
      nothing
      (let ((right-frames 
	     (filter
	      (lambda (frame)
		;; TODO Maybe the inside-transferrer needs to do
		;; something like this too, to avoid creating copies
		;; when all inputs are nothing.
		(not (every nothing?
			    (map (lambda (vcs)
				   ;; TODO direct- or full-?
				   (direct-frame-content vcs frame))
				 outside-vcs))))
	      (good-frames outside-vcs))))
	(alist->virtual-copies
	 (map (lambda (frame)
		(cons frame nothing))
	      (map cdr (select-submap frame-map right-frames)))))))

;;;; Dynamic call sites

(define (dynamic-call-site closure-cell outside-cells)
  (let ((frame-map-cell (make-named-cell 'frame-map))
	(frame-map (make-frame-map '() '())))
    (add-content frame-map-cell frame-map)
    (propagator (cons* frame-map-cell closure-cell outside-cells)
      (eq-label!
       (letrec ((the-propagator
		 (lambda ()
		   (do-the-dynamic-do
		    frame-map-cell closure-cell outside-cells the-propagator))))
	 the-propagator)
       'name 'dynamic-call-manager
       'inputs (cons* frame-map-cell closure-cell outside-cells)
       ;; These outputs are not really right.  It has i/o with the
       ;; cells in the closure; and which of the boundary cells are
       ;; actually written also depends on which closures flow in.
       'outputs (cons* frame-map-cell outside-cells)))))

(define (do-the-dynamic-do frame-map-cell closure-cell outside-cells self)
  (define (update-map frame parents)
    (add-content frame-map-cell
      (simple-ensure-mapping (content frame-map-cell) frame parents)))
  (define (build-interior-occurrences closure target-frame)
    (for-each (lambda (cell)
		(add-content cell (alist->virtual-copies
				   `((,target-frame . ,nothing)))))
	      (v-closure-interior closure)))
  (define (transfer-inward frame target-frame outside-cells inside-cells)
    (for-each
     (lambda (out-cell in-cell)
       (add-content in-cell
	 (alist->virtual-copies
	  `((,target-frame .
			   ,(full-frame-content (content out-cell) frame))))))
     outside-cells
     inside-cells))
  (define (transfer-outward frame target-frame outside-cells inside-cells)
    (for-each
     (lambda (out-cell in-cell)
       (add-content out-cell
	 (alist->virtual-copies
	  `((,(the-occurring-parent frame (content out-cell)) .
	     ,(full-frame-content (content in-cell) target-frame))))))
     outside-cells
     inside-cells))
  (if (or (nothing? (content closure-cell))
	  (any nothing? (map content outside-cells)))
      'done
      (for-each
       (lambda (frame)
	 (let ((closure (full-frame-content (content closure-cell) frame)))
	   ;; TODO At this point, closure is an arbitrary partial
	   ;; information structure, presumably over closures.  To do this
	   ;; properly, I would need to unpack it.  The contents of the
	   ;; outside-cells I can just transfer, however.
	   (if (or (nothing? closure)
		   (every nothing?
			  (map (lambda (vcs)
				 (direct-frame-content vcs frame))
			       (map content outside-cells))))
	       'done
	       (begin
		 (update-map frame (v-closure-default-parents closure))
		 (let ((target-frame (frame-map-get (content frame-map-cell) frame)))
		   (build-interior-occurrences closure target-frame)
		   (transfer-inward
		    frame target-frame outside-cells (v-closure-inside closure))
		   (transfer-outward
		    frame target-frame outside-cells (v-closure-inside closure)))
		 ;; TODO Wow, what a hack!  But yes, the transfer
		 ;; outward portion of this does need to wake up when
		 ;; the interior of the closure gets an answer.  I
		 ;; should separate that out, and make it properly
		 ;; conditioned on the closure remaining the same, and
		 ;; everything, but this conservative extension will
		 ;; do for now.
		 (for-each (lambda (cell)
			     (new-neighbor! cell self))
			   (v-closure-inside closure))))))
       (good-frames (cons (content closure-cell)
			  (map content outside-cells))))))

(define (v-closure-emitter boundary interior output)
  (propagator output
    (eq-label!
     (lambda ()
       (if (not (nothing? (content output)))
	   (add-content output
	     (alist->virtual-copies
	      (map (lambda (frame-content)
		     (cons (car frame-content)
			   (make-v-closure
			    boundary interior (list (car frame-content)))))
		   (virtual-copies->alist (content output)))))))
     'name 'v-closure-emitter
     'inputs (list output)
     'outputs (list output))))

(define (merge-v-closures cl1 cl2)
  (if (and (equal? (v-closure-interior cl1) (v-closure-interior cl2))
	   (equal? (v-closure-inside cl1) (v-closure-inside cl2))
	   (equal? (v-closure-default-parents cl1) (v-closure-default-parents cl2)))
      cl1
      the-contradiction))

(defhandler merge merge-v-closures v-closure? v-closure?)
