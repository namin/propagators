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

;;;; Precise selective profiler a la axch.

;;; The problem this is trying to solve is that most profilers either
;;; waste a huge amount of effort _counting_ everything, or sacrifice
;;; accuracy for efficiency by _sampling_ everything.  Both approaches
;;; also suffer from the fact that the results usually contain much
;;; too much data, and great implementation energy must be expended on
;;; presenting that data to the user in a useful way.

;;; In contrast, the current approach is to count only what the
;;; progammer wants counted.  This saves both runtime labor collecting
;;; data, and post-run labor presenting it, without sacrificing the
;;; exactness of the results.  It also means that tiny runs are
;;; meaningful, because the profiler counts semantic events rather
;;; than random samples.  The downside, of course, is that the
;;; programmer must tell the profiler what to count, and must do so in
;;; advance.  At present, that telling unfortunately happens with
;;; breadcrumbs in the source code; but that might be fixable.

;;; The counts are kept in an addressable tree: 
;;; (foo 23 (bar 34) (baz 22)) means 
;;; (foo bar) counted 34, (foo baz) counted 22, and (foo) itself counted 23.
;;; 
;;; You count things with prof:count:
;;; 
;;;   (prof:count (foo))
;;;     will increase the count of (foo) by 1 every time it runs
;;;   (prof:count (foo bar))
;;;     stats are path addressable: this will increase the count of
;;;     (foo bar) by 1 every time it runs
;;;   (prof:count (foo bar) x)
;;;     will increase the count of (foo bar) by x every time it runs,
;;;     evaluating x each time.
;;;   (prof:count (foo quux) x (lambda (value) ...))
;;;     will increase the count of (foo quux) by x (which can be 0)
;;;     and give runtime access to the value of the (foo quux) counter
;;;     by passing it to the supplied procedure.  This is useful for
;;;     instance for progress reports.
;;; 
;;; Prof:count is a macro so it can avoid the cost of looking up the
;;; storage site for a counter again and again.  However, it supports
;;; dynamic selection of (the tail of) said storage site:
;;; 
;;;   (prof:count (foo ,bar))  ; note the unquote
;;;     will evaluate the expression bar every time it runs, and use
;;;     that to determine which count under foo to increment.  Note
;;;     that the constant part of the path is still looked up at
;;;     macroexansion time.  This is useful for instance for building
;;;     histograms.
;;; 
;;; You access the counts with functions:
;;; 
;;;   (prof:stats [path])
;;;     returns the stats under the optional path (or all of them
;;;     if called without a path argument).  This data structure is
;;;     mutable, affecting the counts.
;;;   (prof:show-stats [path])
;;;     pretty prints the counts under the optional path, eliding
;;;     subtrees that contain no non-zero counts.
;;;   (prof:reset-stats! [path])
;;;     resets all the stats under the optional path to zero.
;;;   (prof:with-reset [path] thunk)
;;;     resets the stats under the optional path, runs the thunk,
;;;     prints out the stats the thunk produced, and returns the
;;;     thunk's value.  Yes, the path is before the thunk in the order
;;;     of arguments, even though the path is optional but the thunk
;;;     is required.  TODO dynmaic-wind to re-clear on entry and print
;;;     on exit??

;;; TODO Pending features:

;;;   (prof:with-fresh-stats [path] thunk)
;;;     like prof:with-reset, but restores the original stats too,
;;;     fluid-let style.  I wonder how feasible this really is.
;;;   (prof:time (quux) body)
;;;     will add the amount of time body takes to (quux) and return
;;;     the value of body, of course.  Can I make it tail-recursive?
;;; I wonder whether there's a more general structure for computing
;;; things about the execution of something than just timing it...

(declare (usual-integrations make-cell cell?))

;;; The statistics data structure is a trie, with an extra
;;; optimization to small integer keys (for histograms).  Every node
;;; is a list whose first element is the count for that node, second element
;;; is a (possibly empty) alist of general subnodes and whose third element
;;; vector of subnodes named by small integers.  The path to a node is
;;; not stored in the node.  The top level is a single nameless root
;;; node.

(define *prof:vector-size* 50)

(define (prof:make-node)
  (list 0 '() (make-vector *prof:vector-size* #f)))

(define prof:node-count car)

(define (prof:node-increment node amount)
  (set-car! node (+ amount (prof:node-count node))))

(define prof:node-children cadr)
(define prof:node-numbered-children caddr)

(define (prof:node-add-child! node name)
  (let ((answer (prof:make-node)))
    (if (and (fixnum? name)
	     (fix:< name *prof:vector-size*))
	(vector-set! (prof:node-numbered-children node)
		     name answer)
	(set-car! (cdr node)
		  (cons (cons name answer)
			(prof:node-children node))))
    answer))

(define *prof:statistics* (prof:make-node))

(define (prof:node-children-as-alist node)
  (append
   (prof:node-children node)
   (filter cdr
	   (map cons (iota *prof:vector-size*)
		(vector->list (prof:node-numbered-children node))))))

(define (prof:node-reset! node)
  (if node
      (begin 
	(set-car! node 0)
	(for-each (lambda (child-pair)
		    (prof:node-reset! (cdr child-pair)))
		  (prof:node-children node))
	(vector-for-each prof:node-reset! (prof:node-numbered-children node)))))

(define (prof:node-get-child node name)
  (if (and (fixnum? name)
	   (fix:< name *prof:vector-size*))
      (vector-ref (prof:node-numbered-children node) name)
      (let ((binding (assoc name (prof:node-children node))))
	(and binding (cdr binding)))))

(define (prof:subnode node path)
  (if (null? path)
      node
      ;; This is separated out like this so I can reuse it inside the
      ;; hairy macro.
      (prof:node-child-test node path prof:subnode)))

(define (prof:node-child-test node path continue)
  (let ((child (prof:node-get-child node (car path))))
    (if child
	(continue child (cdr path))
	(continue (prof:node-add-child! node (car path))
		  (cdr path)))))

;;; prof:count is a hairy macro (at least by my standards)

(define-syntax prof:count
  (sc-macro-transformer
   (lambda (form env)
     (case (length form)
       ((1) (incrementation-form env '() 1))
       ((2) (incrementation-form env (cadr form) 1))
       ((3) (incrementation-form env (cadr form)
	     (close-syntax (caddr form) env)))
       ((4) (incrementation-form env (cadr form)
	     (close-syntax (caddr form) env)
	     (close-syntax (cadddr form) env)))
       (else
	(error "Strangely shaped call to prof:increment" form))))))

(define (incrementation-form use-env path-form inc-form #!optional continue-form)
  (let* ((node-form (node-access-form path-form use-env))
	 (incrementation-form `(prof:node-increment ,node-form ,inc-form)))
    (if (default-object? continue-form)
	incrementation-form
	`(begin ,incrementation-form
		(,continue-form (prof:node-count ,node-form))))))

(define (node-access-form path-form use-env)
  (let loop ((node *prof:statistics*)
	     (path-form path-form))
    (cond ((null? path-form)
	   (emit-unevaluated-reference node))
	  ((and (pair? path-form)
		(pair? (car path-form))
		(eq? (caar path-form) 'unquote)) ;; TODO I bet this isn't stable
	   `(prof:subnode 
	     ,(emit-unevaluated-reference node)
	     ,(close-syntax (list 'quasiquote path-form) use-env)))
	  ((pair? path-form)
	   (prof:node-child-test node path-form loop))
	  (else
	   (error "Malformed path form for prof:increment" path-form)))))

(define *name-count* 0)
(define (fresh-symbol)
  (set! *name-count* (+ *name-count* 1))
  (symbol 'name- *name-count*))

(define (emit-unevaluated-reference thing)
  ;; TODO How can I return an unevaluated pair I have in my hand,
  ;; for purposes of mutation?

  ;; This works in the interpreter, but not if I try to compile
  ;; an invocation of the macro: "object cannot be dumped because it
  ;; contains an environment: #[comment 11]".
  #;
  (list (lambda () thing))

  ;; This works in the interpreter, but if I try to compile
  ;; an invocation of the macro it silently does the wrong thing
  ;; (I assume the macro invocation sites end up with pointers
  ;; that are no longer linked from the main *prof:statistics*).
  
  `(vector-ref ,(vector thing) 0)
  
  ;; This doesn't work at all: "Unbound variable: thing"
  #;
  (capture-syntactic-environment
   (lambda (here-env)
     (close-syntax 'thing here-env)))

  ;; This works in the same session as compiling the macro invocations,
  ;; but fails in the next session with "Unbound variable: name-1"
  #;
  (capture-syntactic-environment
   (lambda (here-env)
     (let ((name (fresh-symbol)))
       (environment-define here-env name thing)
       (close-syntax name here-env))))
  )

;;; So here's a copout.  This lets one run experiments, but the
;;; profiler does, actually cost some visible performance (at least if
;;; one makes nasty histograms).

(define (node-access-form path-form use-env)
  `(prof:subnode
    *prof:statistics*
    ,(close-syntax (list 'quasiquote path-form) use-env)))

;;;; Now that you have stats, what can you do with them?

(define (prof:stats #!optional path)
  (if (default-object? path)
      (set! path '()))
  (prof:subnode *prof:statistics* path))

(define (prof:node-as-alists node)
  (let ((clean-children
	 (map (lambda (child-pair)
		(cons (car child-pair)
		      (prof:node-as-alists (cdr child-pair))))
	      (prof:node-children-as-alist node))))
    (cons (prof:node-count node) clean-children)))

(define (prof:node-clean-copy node)
  (let ((clean-children
	 (filter (lambda (child-pair)
		   (not (null? (cdr child-pair))))
		 (map (lambda (child-pair)
			(cons (car child-pair)
			      (prof:node-clean-copy (cdr child-pair))))
		      (prof:node-children-as-alist node)))))
    (if (= (prof:node-count node) 0)
	clean-children
	(cons (prof:node-count node) clean-children))))

(define (prof:show-stats #!optional path)
  (pp (prof:node-clean-copy (prof:stats path))))

(define (prof:reset-stats! #!optional path)
  (prof:node-reset! (prof:stats path)))

(define (prof:with-reset thunk #!optional path)
  ;; I want the path to be the first argument, even though it's optional
  (if (not (default-object? path))
      (let ((tmp thunk))
	(set! thunk path)
	(set! path tmp))
      (set! path '()))
  (prof:reset-stats! path)
  (let ((value (show-time thunk)))
    (newline)
    (prof:show-stats path)
    value))

;;; Be careful with this! Any (prof:count ...) that macroexpanded
;;; before doing this will become invalid!  (If they point to a node
;;; that this removes).

;;; TODO is prof:clear-stats! even useful?  The various macro-expanded
;;; prof:count calls will still hold on to their nodes, so this won't
;;; allow anything interesting to get garbage collected.  Perhaps one
;;; could write a version that recursively unlinked every node from
;;; every other, in order to be able to garbage-collect histograms.
;;; As to keeping unused stats from showing up in a display, perhaps
;;; prof:show-stats is a better place for doing that...
(define (prof:clear-stats! #!optional path)
  (prof:node-clear! (prof:stats path)))

(define (prof:node-clear! node)
  (set-car! node 0)
  (set-car! (cdr node) '())
  (set-car! (cddr node) (make-vector *prof:vector-size* #f)))

;;; TODO prof:remove-node!
