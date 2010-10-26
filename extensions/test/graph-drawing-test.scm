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

 (define-test (naming-smoke)
   (initialize-scheduler)
   (define-cell foo)
   (define-cell bar)
   (define-cell baz)
   (p:+ foo bar baz)
   (check (= 1 (length (neighbors foo))))
   (check (= 1 (length (neighbors bar))))
   (check (= 0 (length (neighbors baz))))
   (check (eq? 'foo (name foo)))
   (check (eq? 'bar (name bar)))
   (check (eq? 'baz (name baz)))
   (define the-adder (car (neighbors foo)))
   (check (eq? the-adder (car (neighbors bar))))
   (check (equal? (list foo bar) (propagator-inputs the-adder)))
   (check (equal? (list baz) (propagator-outputs the-adder)))
   (check (eq? '+ (name the-adder)))
   (check (propagator? the-adder))
   (check (cell? foo))
   (check (cell? bar))
   (check (cell? baz)))

 (define-test (drawing-smoke)
   (interaction
    (force-hash-number 200)
    (initialize-scheduler)
    (define-cell foo)
    (define-cell bar)
    (p:id foo bar)
    (draw:write-graph-to-string foo)
    (check (equal?
"digraph G {
  ratio=fill;
  \"cell-201\" [label=\"foo\", shape=\"ellipse\" ];
  \"prop-202\" [label=\"identity\", shape=\"box\" ];
  \"cell-201\" -> \"prop-202\" [label=\"\" ];
  \"prop-202\" -> \"cell-203\" [label=\"\" ];
  \"cell-203\" [label=\"bar\", shape=\"ellipse\" ];
}
" (out)))
    (check (equal? (draw:write-graph-to-string foo)
		   (draw:write-graph-to-string (list foo bar))))))
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
    (with-network-group (network-group-named 'subgroup)
      (lambda ()
	(define-cell bar)
	(p:id foo bar)))
    (draw:write-graph-to-string *current-network-group*)
    (check (equal?
"digraph G {
  ratio=fill;
  subgraph cluster_215 { label=\"top-group\"; 
    subgraph cluster_216 { label=\"subgroup\"; 
      \"prop-217\" [label=\"identity\", shape=\"box\" ];
      \"cell-219\" [label=\"bar\", shape=\"ellipse\" ];
    }
    \"cell-218\" [label=\"foo\", shape=\"ellipse\" ];
  }
  \"cell-218\" -> \"prop-217\" [label=\"\" ];
  \"prop-217\" -> \"cell-219\" [label=\"\" ];
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
  subgraph cluster_240 { label=\"top-group\"; 
    subgraph cluster_241 { label=\"c:id\"; 
      \"prop-242\" [label=\"identity\", shape=\"box\" ];
      \"prop-245\" [label=\"identity\", shape=\"box\" ];
    }
    \"cell-243\" [label=\"bar\", shape=\"ellipse\" ];
    \"cell-244\" [label=\"foo\", shape=\"ellipse\" ];
  }
  \"cell-243\" -> \"prop-242\" [label=\"\" ];
  \"prop-242\" -> \"cell-244\" [label=\"\" ];
  \"cell-244\" -> \"prop-245\" [label=\"\" ];
  \"prop-245\" -> \"cell-243\" [label=\"\" ];
}
" (out)))))

 (define-test (macrology-smoke)
   (initialize-scheduler)
   (let-cells ((foo (make-cell))
	       bar
	       (baz (make-cell)))
     (check (eq? 'foo (name foo)))
     (check (not (eq-get foo 'name)))
     (check (eq? 'bar (name bar)))
     (check (eq? 'bar (eq-get bar 'name)))
     (check (eq? 'baz (name baz)))
     (check (not (eq-get baz 'name)))
     ))

;;; TODO more-macrology-smoke-2 and expression-substructure-test fail
;;; to syntax in at least some versions of mechanics because of a
;;; macro-expander bug.
#;
 (define-test (more-macrology-smoke-2)
   (initialize-scheduler)
   (define-propagator (frobnicate frob)
     (check (not (network-group-contains? *current-network-group* frob)))
     (check (eq? 'frob (local-name frob)))
     (check (eq? 'foo (name frob))))
   (define-cell foo)
   (check (network-group-contains? *current-network-group* foo))
   (check (eq? 'foo (name foo)))
   (check (eq? 'foo (local-name foo)))
   (p:frobnicate foo))
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
