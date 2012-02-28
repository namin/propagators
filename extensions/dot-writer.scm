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

(define (make-dot-writer output-port)
  (define (write-graph write-contents)
    (write-string "digraph G {" output-port)
    (newline output-port)
    (draw:indented
     (lambda ()
       (write-options)
       (write-contents)))
    (write-string "}" output-port)
    (newline output-port))

  (define (write-options)
    (for-each (lambda (option)
		(write-indentation)
		(write-string option output-port)
		(write-string ";" output-port)
		(newline output-port))
	      '(; "orientation=landscape"
                ; "size=\"10,7.5\""
                ; "page=\"8.5,11\""
		"ratio=fill")))

  (define (do-write-node node-id attributes)
    (write-indentation)
    (write node-id output-port)
    (write-attributes attributes)
    (write-string ";" output-port)
    (newline output-port))

  (define (node-shape node)
    (cond ((cell? node) "ellipse")
	  ((diagram? node) "box")
	  (else
	   (error "Unshapeable node type" node))))

  (define (write-node node)
    (do-write-node
     (draw:node-id node)
     `(("label" . ,(draw:node-label node))
       ("shape" . ,(node-shape node)))))

  (define (write-edge source-name target-name label)
    (write-indentation)
    (write source-name output-port)
    (write-string " -> " output-port)
    (write target-name output-port)
    (write-attributes `(("label" . ,label)))
    (write-string ";" output-port)
    (newline output-port))

  (define (write-cluster id label write-contents)
    (write-subgraph
     (string-append "cluster_" (write-to-string id))
     label write-contents))

  (define (write-subgraph id label write-contents)
    (write-indentation)
    (write-string "subgraph " output-port)
    (write-string id output-port)
    (write-string " { " output-port)
    (write-subgraph-attributes `(("label" . ,(write-to-string label))))
    (newline output-port)
    (draw:indented write-contents)
    (write-indentation)
    (write-string "}" output-port)
    (newline output-port))

  (define (write-attributes attributes)
    (if (pair? attributes)
	(let ((first-attribute? #t))
	  (write-string " [" output-port)
	  (for-each (lambda (attribute)
		      (if (not first-attribute?)
			  (write-string ", " output-port))
		      (write-string (car attribute) output-port)
		      (write-string "=" output-port)
		      (write (cdr attribute) output-port)
		      (set! first-attribute? #f))
		    attributes)
	  (write-string " ]" output-port))))

  ;;; TODO Why is the string handling in MIT Scheme so awful?
  (define (write-subgraph-attributes attributes)
    (if (pair? attributes)
	(for-each (lambda (attribute)
		    (write-string (car attribute) output-port)
		    (write-string "=" output-port)
		    (write (cdr attribute) output-port)
		    (write-string "; " output-port))
		  attributes)))

  (define (write-indentation)
    (repeat draw:indentation-level
	    (lambda ()
	      (write-string "  " output-port))))

  (define (me message)
    (cond ((eq? 'write-graph message) write-graph)
	  ((eq? 'write-node message) write-node)
	  ((eq? 'write-edge message) write-edge)
	  ((eq? 'write-cluster message) write-cluster)
	  (else
	   (error "Unknown message" message))))
  
  me)
