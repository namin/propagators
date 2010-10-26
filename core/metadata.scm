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

;;;; Network Metadata

;;; The purpose of this steaming pile is to allow the collection of
;;; metadata sufficient to traverse and inspect a running propagator
;;; network, for the purpose of debugging it.  As an extreme case,
;;; extensions/draw.scm uses the collected metadata to draw pictures
;;; of (small, simple) networks in dot and yFiles.

(define (propagator-inputs propagator)
  (or (eq-get propagator 'inputs)
      (eq-get propagator 'neighbors)
      '()))

(define (propagator-outputs propagator)
  (or (eq-get propagator 'outputs)
      (eq-get propagator 'neighbors)
      '()))

(define (cell-non-readers cell)
  (or (eq-get cell 'shadow-connections)
      '()))

(define (cell-connections cell)
  ;; The neighbors are the ones that need to be woken up; the
  ;; connections are the ones that touch the cell at all.  This
  ;; concept is useful for walking the graph structure of the network.
  (append (neighbors cell) (cell-non-readers cell)))

(define-structure (network-group (safe-accessors #t))
  elements
  names)

(define *current-network-group* #f)

(define (network-group-named name)
  (name! (make-network-group '() (make-eq-hash-table)) name))

(define (name-in-group! group thing name)
  (hash-table/put! (network-group-names group) thing name)
  thing)

(define (name-in-group group thing)
  (and group
       (hash-table/get (network-group-names group) thing #f)))

(define (network-register thing)
  (if (memq thing (network-group-elements *current-network-group*))
      'ok
      (set-network-group-elements! *current-network-group*
       (cons thing (network-group-elements *current-network-group*))))
  (eq-put! thing 'network-group *current-network-group*))

(define (network-unregister thing)
  (let ((group (network-group-of thing)))
    (if group
	(set-network-group-elements! group
	 (delq thing (network-group-elements group)))))
  (eq-rem! thing 'network-group))

(define (network-group-of thing)
  (eq-get thing 'network-group))

(define (network-group-contains? group thing)
  (or (eq? group (network-group-of thing))
      (and (network-group-of thing)
	   (network-group-contains? group (network-group-of thing)))))

(define (in-network-group group thunk)
  (if group
      (fluid-let ((*current-network-group* group))
	(thunk))
      (thunk) ;; TODO What should I really do if there is no group?
      ))

(define (with-network-group group thunk)
  (network-register group)
  (in-network-group group thunk))

(define (name-locally! thing name)
  (name-in-group! *current-network-group* thing name))

(define (local-name thing)
  (name-in-group *current-network-group* thing))

(define name
  (let ((name name))
    (lambda (thing)
      (let ((group-name (name-in-group (network-group-of thing) thing)))
	(if group-name
	    (name group-name)
	    (name thing))))))

(define (clear-network-group thing)
  (eq-rem! thing 'shadow-connections 'inputs 'outputs 'network-group)
  (if (network-group? thing)
      (for-each clear-network-group (network-group-elements thing))))

(define (reset-network-groups!)
  (clear-network-group *current-network-group*)
  (set! *current-network-group* (network-group-named 'top-group)))

(define initialize-scheduler
  (let ((initialize-scheduler initialize-scheduler))
    (lambda ()
      (initialize-scheduler)
      (reset-network-groups!))))

(define with-independent-scheduler
  (let ((with-independent-scheduler with-independent-scheduler))
    (lambda args
      (fluid-let ((*current-network-group* #f))
	(apply with-independent-scheduler args)))))

;;; Oof!
;;; TODO Figure out what network-group-expression-substructure is
;;; really doing and refactor it.
(define (network-group-expression-substructure group)
  ;; Produce the collection of cells, propagators, network groups, and
  ;; expression network groups that should be visible at this group
  ;; level.  This may involve constructing network groups that
  ;; represent expressions made with e: constructs, on the logic that
  ;; the cells they create are implicit, and therefore should be
  ;; hidden (unless the use explicitly expands the autogenerated
  ;; expression group that contains them).
  (define (should-hide? thing)
    (and (cell? thing)
	 (eq? group (network-group-of thing))
	 (not (name-in-group group thing))))
  (define (should-not-hide? thing)
    (and (cell? thing)
	 (or (not (eq? group (network-group-of thing)))
	     (name-in-group group thing))))
  (define (may-hide? thing)
    (and (not (should-hide? thing))
	 (not (should-not-hide? thing))))
  (define (connected? thing1 thing2)
    (define (connected-to-cell? cell thing)
      (and (cell? cell)
	   (or (memq thing (cell-connections cell))
	       (and (network-group? thing)
		    (any (lambda (conn)
			   (network-group-contains? thing conn))
			 (cell-connections cell))))))
    (or (connected-to-cell? thing1 thing2)
	(connected-to-cell? thing2 thing1)))
  (define (make-subgroup elements)
    (name!
     (make-network-group elements (make-eq-hash-table))
     (compute-expression-name elements)))
  (define (compute-expression-name elements)
    (define functionalized-tags (make-eq-hash-table))
    (define (connections-of thing)
      (if (cell? thing)
	  (cell-connections thing)
	  (filter (lambda (other)
		    (connected? thing other))
		  (delete-duplicates
		   (append (network-group-elements group)
			   ;; TODO Oops!  Travesing keys of a weak table!
			   (hash-table/key-list
			    (network-group-names group)))))))
    (define (functionalized-to thing)
      (and (not (cell? thing))
	   (let ((connections (connections-of thing)))
	     ;; TODO Heuristic, and only works on single-output
	     ;; functionalized things.  It wouldn't have worked to
	     ;; just tag them at functionalization time because
	     ;; functionalize sees the propagator constructor, but
	     ;; these things are the constructed propagators.
	     #;
	     (pp (list (name thing)
		       (hash thing)
		       (map (lambda (c)
			      (list (name c) (hash c)
				    (and (eq-get c 'subexprs)
					 (map (lambda (s)
						(list (name s) (hash s)))
					      (eq-get c 'subexprs)))))
			    connections)))
	     (any (lambda (connection)
		    (and (eq-get connection 'subexprs)
			 (lset= eq? (eq-get connection 'subexprs)
				(delq connection connections))
			 connection))
		  connections))))
    (define (functionalized-tag! thing)
      (let ((target (functionalized-to thing)))
	(if target
	    (hash-table/put! functionalized-tags thing target))))
    (define (functionalized? thing)
      (memq (hash-table/get functionalized-tags thing #f)
	    elements))
    (define (functionalized-to-me cell)
      (and (cell? cell)
	   (find (lambda (thing)
		   (eq? cell (hash-table/get functionalized-tags thing #f)))
		 elements)))
    (for-each functionalized-tag! elements)
    #; (pp (hash-table->alist functionalized-tags))
    (let loop ((head (find (lambda (thing)
			     (and (not (cell? thing))
				  (not (functionalized? thing))))
			   elements)))
      #; (pp `(,(name head) ,(hash head)))
      (if (cell? head)
	  (if (and (memq head elements)
		   (eq-get head 'subexprs))
	      (cons (name (functionalized-to-me head))
		    (map loop (eq-get head 'subexprs)))
	      (name-in-group group head))
	  (cons (name head) (map loop (lset-intersection eq?
					(connections-of head)
					elements))))))

  (let loop ((target-subgroups
	      (map list (filter should-hide?
				(network-group-elements group))))
	     (hidable-elements
	      (map list (filter may-hide?
				(network-group-elements group))))
	     (shown-elements
	      (filter should-not-hide? (network-group-elements group))))
    (define (find-pair-to-merge)
      (let per-subgroup ((subgroups target-subgroups))
	(if (null? subgroups)
	    #f
	    (let per-element ((elements (car subgroups)))
	      (if (null? elements)
		  (per-subgroup (cdr subgroups))
		  (let ()
		    (define (wanted? pile)
		      (any (lambda (thing)
			     (connected? (car elements) thing))
			   pile))
		    (cond ((find wanted? (cdr subgroups)) =>
			   (lambda (wanted-subgroup)
			     (cons elements wanted-subgroup)))
			  ((find wanted? hidable-elements) =>
			   (lambda (wanted-subgroup)
			     (cons elements wanted-subgroup)))
			  (else (per-element (cdr elements))))))))))
    (let ((pair-to-merge (find-pair-to-merge)))
      (if pair-to-merge
	  (loop (cons (delete-duplicates
		       ;; I don't get why I need this delete-duplicates,
		       ;; but without it the substructure mysteriously
		       ;; repeats elements.
		       (append (car pair-to-merge)
			       (cdr pair-to-merge)))
		      (delq (car pair-to-merge)
			    (delq (cdr pair-to-merge)
				  target-subgroups)))
		(delq (car pair-to-merge)
		      (delq (cdr pair-to-merge)
			    hidable-elements))
		shown-elements)
	  (append (map make-subgroup target-subgroups)
		  (map car hidable-elements)
		  shown-elements)))))

;;; Stuff for automatically determining the i/o characteristics of a
;;; compound box by expanding it out (in a sandbox) and looking at the
;;; i/o characteristics of its structure.

(define *interesting-cells* #f)

(define (compute-aggregate-metadata prop-ctor arg-cells)
  ;; This check is here to keep recursive compounds from computing
  ;; their internal metadata forever.  The reason this is ok is that
  ;; to learn the metadata of an unexpanded box, I only need to
  ;; observe what propagators want to attach to its interior boundary,
  ;; not to the entire interior.
  (if (or (not *interesting-cells*)
	  (not (null? (lset-intersection eq?
                        *interesting-cells* arg-cells))))
      (do-compute-aggregate-metadata prop-ctor arg-cells)
      '()))

(define (do-compute-aggregate-metadata prop-ctor arg-cells)
  ;; Assumes the prop-ctor is stateless!
  (with-independent-scheduler
   (lambda ()
     (let ((test-cell-map (map (lambda (arg)
				 (cons arg (make-cell)))
			       arg-cells)))
       (fluid-let ((*interesting-cells* (map cdr test-cell-map)))
	 (apply prop-ctor (map cdr test-cell-map)))
       (let* ((the-props (all-propagators))
	      (inputs (apply append (map (lambda (prop)
					   (or (eq-get prop 'inputs)
					       '()))
					 the-props)))
	      (outputs (apply append (map (lambda (prop)
					    (or (eq-get prop 'outputs)
						'()))
					  the-props)))
	      (my-inputs (map car
			      (filter (lambda (arg-test)
					(memq (cdr arg-test) inputs))
				      test-cell-map)))
	      (my-outputs (map car
			       (filter (lambda (arg-test)
					 (memq (cdr arg-test) outputs))
				       test-cell-map)))
	      (constructed-objects ;; Should only be one
	       (filter (lambda (x) (not (cell? x)))
		       (network-group-elements *current-network-group*))))
	 `(name ,(name (car constructed-objects))
	   inputs ,my-inputs outputs ,my-outputs))))))
