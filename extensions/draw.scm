;;; ----------------------------------------------------------------------
;;; Copyright 2009-2010 Taylor Campbell and Alexey Radul.
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

;;;; Code to visualize propagator networks as graphs.

;;; Uses:
;;; 
;;; (draw:show-graph)
;;;   Dumps the whole current network in dot format, runs the dot
;;;   graph layout engine over it, and displays the resulting svg file
;;;   with eog.  Errors out if dot or eog are not available.
;;; 
;;; (draw:show-graph <thing>)
;;;   As above, but shows the portion of the network identified by
;;;   <thing>.  If <thing> is a cell or a propagator, draws the
;;;   connected component containing that cell or propagator.  If
;;;   <thing> is a network group, draws the elements that are part of
;;;   that network group.
;;; 
;;; (draw:show-graph <thing> <drawing-program>)
;;;   As above, but uses the specified drawing program (indicated as a
;;;   string) instead of dot.  As of the present writing, the graphviz
;;;   system ships with the programs dot, neato, twopi, circo, and
;;;   fdp, which embody different graph layout algorithms.
;;; 
;;; (draw:draw-graph-to-file <filename>)
;;; (draw:draw-graph-to-file <filename> <thing>)
;;; (draw:draw-graph-to-file <filename> <thing> <drawing-program>)
;;;   Saves the graph layout output in the given <filename>, but does
;;;   not display it.  Same treatment of <thing> as above.
;;; 
;;; (draw:write-graph)
;;; (draw:write-graph <thing>)
;;; (draw:write-graph <thing> <output-port>)
;;; (draw:write-graph-to-file <filename>)
;;; (draw:write-graph-to-file <filename> <thing>)
;;; (draw:write-graph-to-string)
;;; (draw:write-graph-to-string <thing>)
;;;   Writes the graph to the specified output location (either the
;;;   standard output, or the given port, or the given file, or a
;;;   fresh string it then returns).  Does not process the output with
;;;   any external programs.
;;; 
;;; The behavior of the above can be modulated by fluid-letting
;;; several variables:
;;; 
;;; draw:cell-label
;;;   A procedure that accepts a cell and emits a structure that will
;;;   be passed through write-to-string to generate the label that
;;;   cell should have in the output.  Defaults to the procedure name.
;;; 
;;; draw:propagator-label
;;;   A procedure that accepts a propagator and emits a structure that
;;;   will be passed through write-to-string to generate the label
;;;   that cell should have in the output.  Defaults to name.
;;; 
;;; draw:format
;;;   A symbol that specifies the output format.  Currently supported
;;;   formats are:
;;;   dot, for the graphviz suite (this is the default)
;;;   graphml, for yEd
;;; 
;;; For example,
#;
 (fluid-let ((draw:cell-label
	      (lambda (var) (cons (name var) (content var)))))
   (draw:show-graph))
;;; will display the network, laid out with dot, but including the
;;; contents of all cells in addition to their names.
;;; 
#;
 (fluid-let ((draw:format 'graphml))
   (draw:write-graph-to-file "frob.graphml"))
;;; will write the graph structure of the whole propagator network to
;;; the file "frob.graphml" in graphml format, which can then be
;;; viewed with yEd (if that program is available).

;;; TODOs:
;;; Write a handful of useful procedures to fluid bind draw:cell-label to
;;; Dump port data (this compound box takes these inputs and then
;;;   routes them to these sub-boxes)
;;; Dump subgroup data for closures ??
;;; - Implement (draw:show-graph some-closure)
;;; Dump animations of the progress of values over time
;;; - ddb searches; recursions
;;; Draw pictures of all the interesting propagator networks.
;;; Explore various graph drawing engines: graphviz, JGraph, others.
;;;   http://www2.research.att.com/~volinsky/Graphs/slides/north.pdf

(define (draw:show-graph #!optional start drawing-program)
  (call-with-temporary-file-pathname
   (lambda (svg-pathname)
     (draw:draw-graph-to-file svg-pathname start drawing-program)
     ;; TODO There is, in principle, support for asynchronous
     ;; subprocesses, but it is "available for those who are willing
     ;; to read the source code."  More on this in an email exchange
     ;; with Taylor titled "Happy New Year, and a Question"
     (force-run-shell-command
      (string-append "eog " (->namestring svg-pathname))))))

(define (draw:draw-graph-to-file pathname #!optional start drawer)
  (if (default-object? drawer)
      (set! drawer "dot"))
  (call-with-temporary-file-pathname
   (lambda (graph-pathname)
     (draw:write-graph-to-file graph-pathname start)
     (force-run-shell-command
      (string-append
       drawer " " (->namestring graph-pathname)
       " -Tsvg -o " (->namestring pathname))))))

(define (force-run-shell-command command)
  (let ((status (run-shell-command command)))
    (if (= 0 status)
	'ok
	(error "Shell command failed" command))))

(define (draw:write-graph-to-file pathname #!optional start)
  (call-with-output-file pathname
    (lambda (output-port)
      (draw:write-graph start output-port))))

(define (draw:write-graph-to-string #!optional start)
  (call-with-output-string
   (lambda (output-port)
     (draw:write-graph start output-port))))

(define (draw:write-graph #!optional start output-port)
  (if (default-object? output-port)
      (set! output-port (current-output-port)))
  (let ((writer (draw:make-writer output-port)))
    ((writer 'write-graph)
     (lambda ()
       (draw:walk-graph writer start)))))

(define draw:format 'dot)

(define (draw:make-writer output-port)
  ((case draw:format
     ((dot) make-dot-writer)
     ((graphml) make-graphml-writer)
     (else (error "Unsupported drawing format" draw:format)))
   output-port))

(define (draw:walk-graph writer #!optional start)
  (let ((traversed (make-eq-hash-table))
	(defer-edges? #f)
	(deferred-edges '()))
    ;; TODO Handle circumstances when the same diagram is a part of
    ;; several clubs.

    (define write-node (writer 'write-node))

    (define (write-edge source target label)
      (define (edge-writer)
	((writer 'write-edge) source target label))
      (if defer-edges?
	  (set! deferred-edges (cons edge-writer deferred-edges))
	  (edge-writer)))

    (define (write-input-edge input name index)
      (write-edge input name index))

    (define (write-output-edge output name index)
      (write-edge name output index))

    (define (write-edges diagram accessor write-edge)
      (let ((name (draw:node-id diagram))
	    (number-edges? (< 1 (length (accessor diagram)))))
        (let loop ((cells (accessor diagram)) (index 0))
          (if (pair? cells)
              (let ((cell (car cells)))
                (write-edge (draw:node-id cell) name (if number-edges? index ""))
                (loop (cdr cells) (+ index 1)))))))

    (define (write-apex diagram)
      (write-node diagram)
      (write-edges diagram diagram-inputs write-input-edge)
      (write-edges diagram diagram-outputs write-output-edge))

    ;; TODO Implement levels of detail in the graph drawing.  An
    ;; unexpanded compound should have good arrows to its external
    ;; parts.
    (define (traverse-group group)
      (fluid-let ((defer-edges? #t))
	((writer 'write-cluster) (hash group) (name group)
	 (lambda ()
	   (for-each traverse (diagram-expression-substructure group)))))
      (if (not defer-edges?)
	  (dump-deferred-edges)))

    (define (traverse thing)
      (if (hash-table/get traversed thing #f)
	  'ok
	  (begin
	    (hash-table/put! traversed thing #t)
	    (cond ((cell? thing)
		  (write-node thing))
		 ((primitive-diagram? thing)
		  (write-apex thing))
		 ((diagram? thing)
		  (traverse-group thing))
		 (else
		  'ok)))))

    (define (dump-deferred-edges)
      (for-each (lambda (edge-writer) (edge-writer))
		(reverse deferred-edges))
      (set! deferred-edges '()))

    (define (dispatch start)
      (cond ((default-object? start) (traverse-group *toplevel-diagram*))
	    ((diagram? start) (traverse start))
	    ((pair? start) (for-each dispatch start))
	    (else
	     (error "Unknown entry point" start))))

    (dispatch start)))

(define (draw:node-id node)
  (define (node-type-string node)
    (cond ((cell? node) "cell-")
	  ((diagram? node) "prop-")
	  (else
	   (error "Unknown node type" node))))
  (string-append (node-type-string node) (write-to-string (hash node))))

(define (draw:node-label node)
  (write-to-string
   (cond ((cell? node) (draw:cell-label node))
	 ((diagram? node) (draw:diagram-label node))
	 (else
	  (error "Unnameable node type" node)))))

(define draw:indentation-level 0)

(define (draw:indented thunk)
  (fluid-let ((draw:indentation-level
	       (+ draw:indentation-level 1)))
    (thunk)))

(define draw:diagram-label name)
(define draw:cell-label name)
