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

(in-test-group
 graph-drawing

 (define-each-check
   (equal? '+ (name generic-+)))

 (define-test (drawing-smoke)
   (interaction
    (force-hash-number 200)
    (initialize-scheduler)
    (define-cell foo)
    (define-cell bar)
    (p:id foo bar)
    (draw:write-graph-to-string *toplevel-diagram*)
    (check (equal?
"digraph G {
  ratio=fill;
  subgraph cluster_201 { label=\"toplevel\"; 
    \"cell-202\" [label=\"bar\", shape=\"ellipse\" ];
    \"cell-203\" [label=\"foo\", shape=\"ellipse\" ];
    \"prop-204\" [label=\"identity:p\", shape=\"box\" ];
  }
  \"cell-203\" -> \"prop-204\" [label=\"\" ];
  \"prop-204\" -> \"cell-202\" [label=\"\" ];
}
" (out)))))
#|
 ;;; These tests are slow (because they ask for lots of GC) but they
 ;;; don't test much when things are working, because the following
 ;;; test summarizes them.
 (define-each-check
   (< (memory-loss-from (repeated 100 make-eq-hash-table)) 2)
   (< (memory-loss-from (repeated 100 make-strong-eq-hash-table)) 2)
   (< (memory-loss-from (repeated 100 reset-premise-info!)) 2)
   (< (memory-loss-from (repeated 500 reset-network-groups!)) 10)
   (< (memory-loss-from (repeated 100 initialize-scheduler)) 2))

 (define-test (groups-do-not-leak)
   (initialize-scheduler)
   (define (one-small-network)
     (define-cell foo)
     (define-cell bar)
     (initialize-scheduler))
   (check (< (memory-loss-from (repeated 100 one-small-network)) 2)))
|#
;; TODO: leaks
#;
 (define-test (groups-do-not-leak-2)
   (initialize-scheduler)
   (define (one-small-network)
     (define-cell foo)
     (define-cell bar)
     (p:id foo bar)
     (initialize-scheduler))
   (check (< (memory-loss-from (repeated 100 one-small-network)) 2)))

 (define-test (grouped-drawing)
   (interaction
    (force-hash-number 214)
    (initialize-scheduler)
    (define-cell foo)
    (diagram-style-with-diagram (empty-diagram 'subgroup)
      (lambda ()
	(define-cell bar)
	(p:id foo bar)))
    (draw:write-graph-to-string *toplevel-diagram*)
    (check (equal?
"digraph G {
  ratio=fill;
  subgraph cluster_215 { label=\"toplevel\"; 
    \"cell-216\" [label=\"foo\", shape=\"ellipse\" ];
    subgraph cluster_217 { label=\"subgroup\"; 
      \"cell-218\" [label=\"bar\", shape=\"ellipse\" ];
      \"prop-219\" [label=\"identity:p\", shape=\"box\" ];
    }
  }
  \"cell-216\" -> \"prop-219\" [label=\"\" ];
  \"prop-219\" -> \"cell-218\" [label=\"\" ];
}
" (out)))))

 (define-test (grouped-drawing-2)
   (interaction
    (force-hash-number 239)
    (initialize-scheduler)
    (define-cell foo)
    (define-cell bar)
    (c:id foo bar)
    (draw:write-graph-to-string)
    (check (equal?
"digraph G {
  ratio=fill;
  subgraph cluster_240 { label=\"toplevel\"; 
    \"cell-241\" [label=\"bar\", shape=\"ellipse\" ];
    \"cell-242\" [label=\"foo\", shape=\"ellipse\" ];
    subgraph cluster_243 { label=\"c:id\"; 
      \"prop-244\" [label=\"identity:p\", shape=\"box\" ];
      \"prop-245\" [label=\"identity:p\", shape=\"box\" ];
    }
  }
  \"cell-241\" -> \"prop-244\" [label=\"\" ];
  \"prop-244\" -> \"cell-242\" [label=\"\" ];
  \"cell-242\" -> \"prop-245\" [label=\"\" ];
  \"prop-245\" -> \"cell-241\" [label=\"\" ];
}
" (out)))))

;;; TODO expression-substructure-test fails to syntax in at least some
;;; versions of mechanics because of a macro-expander bug.  It is also
;;; outdated, in that it relies on old network-group technology rather
;;; than the new diagram technology.

#;
 (define-test (expression-substructure-test)
   (initialize-scheduler)
   (define-propagator (frobnicate frob)
     (let* ((first-internal (e:+ frob frob))
	    (second-internal (e:+ frob first-internal)))
       (let-cells ((sum (e:+ frob second-internal)))
	 (let ((the-expression-substructure
		(network-group-expression-substructure *current-network-group*)))
	   ;; sum is shown because it has a name
	   (check (memq sum the-expression-substructure))
	   ;; frob is not shown because it's not part of the group at all
	   (check (not (memq frob the-expression-substructure)))
	   ;; first-internal is hidden becuase it's internal to an expression
	   (check (not (memq first-internal the-expression-substructure)))
	   ;; ditto second-internal
	   (check (not (memq second-internal the-expression-substructure)))
	   (check (= 1 (length (filter cell? the-expression-substructure))))
	   ;; all the propagators are hidden because they abut on expressions
	   (check (= 0 (length (filter propagator? the-expression-substructure))))
	   ;; TODO get the names right
	   #;
	   (check
	    (equal? '(+ frob (+ frob (+ frob frob)))
		    (name (car (filter network-group?
				       the-expression-substructure)))))
	   (let ((the-subgroup-structure
		  (network-group-expression-substructure
		   (car (filter network-group? the-expression-substructure)))))
	     ;; But inside the generated subgroup, the internal cell
	     ;; and the propagators it connects are explicit.
	     (check (memq first-internal the-subgroup-structure))
	     (check (memq second-internal the-subgroup-structure))
	     (check (= 3 (length (filter propagator? the-subgroup-structure))))
	     (check (= 2 (length (filter cell? the-subgroup-structure))))
	     (check (= 0 (length (filter network-group? the-subgroup-structure))))
	     )))))
   (p:frobnicate (e:constant 2))
   )

 ;; TODO Add a test of drawing networks with expanded and unexpanded
 ;; compound propagators.

 )
