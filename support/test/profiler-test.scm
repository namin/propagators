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
 profiler
 
 (define-test (smoke)
   (define root (prof:make-node))
   (define sub (prof:subnode root '(foo bar baz)))
   (check (equal? 0 (prof:node-count sub)))
   (check (equal? 0 (prof:node-count root)))
   (check (eq? sub (prof:subnode root '(foo bar baz))))
   (check (equal?
	   '(0 ((foo 0
		     ((bar 0
			   ((baz 0
				 ()
				 #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
			   #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
		     #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
	       #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
	   root))
   (check (equal? '(0 (foo 0 (bar 0 (baz 0)))) (prof:node-as-alists root)))
   (define sub2 (prof:subnode root '(foo quux)))
   (prof:node-increment sub2 7)
   (check (equal? 7 (prof:node-count sub2)))
   (check (equal? '(0 (foo 0 (quux 7) (bar 0 (baz 0))))
		  (prof:node-as-alists root)))
   (prof:node-increment (prof:subnode root '(foo quux)) 3)
   (check (equal? 10 (prof:node-count sub2)))
   (check (equal? '(0 (foo 0 (quux 10) (bar 0 (baz 0))))
		  (prof:node-as-alists root)))
   )

 (define-test (hairy-macro)
   ;; The structure gets built at macro-expansion time, but the
   ;; checks happen at runtime!
   (define my-node (prof:subnode *prof:statistics* '(hairy-macro-test)))
   (define (show-my-node) (prof:node-as-alists my-node))
   (prof:count (hairy-macro-test foo) 0)
   (prof:count (hairy-macro-test bar) 0)
   (prof:count (hairy-macro-test data) 0)
   (check (equal? '(0 (data 0) (bar 0) (foo 0)) (show-my-node)))

   ;; TODO Is there a way to delay macroexpansion so that I can be
   ;; sure that these prof:count macros are not expanded unless
   ;; (and until) the test is actually run?

   ;; Counting
   (prof:count (hairy-macro-test foo))
   (check (equal? '(0 (data 0) (bar 0) (foo 1)) (show-my-node)))

   (prof:count (hairy-macro-test foo))
   (check (equal? '(0 (data 0) (bar 0) (foo 2)) (show-my-node)))

   ;; Counting by steps
   (prof:count (hairy-macro-test bar) (/ 4 2))
   (check (equal? '(0 (data 0) (bar 2) (foo 2)) (show-my-node)))

   ;; Histograms
   (define datum 5)
   (prof:count (hairy-macro-test data ,datum))
   (check (equal? '(0 (data 0 (5 1)) (bar 2) (foo 2)) (show-my-node)))
   (for-each (lambda (item)
	       (set! datum item)
	       (prof:count (hairy-macro-test data ,datum)))
	     '(3 5 2 9 5 2))
   (check (equal? '(0 (data 0 (2 2) (3 1) (5 3) (9 1))
		      (bar 2) (foo 2))
		  (show-my-node)))

   ;; Dynamic subnote
   (prof:count (hairy-macro-test data ,datum subnote))
   (check (equal? '(0 (data 0 (2 2 (subnote 1)) (3 1) (5 3) (9 1))
		      (bar 2) (foo 2))
		  (show-my-node)))
   
   ;; Runtime access to counts
   (prof:count (hairy-macro-test foo) 1
     (lambda (foo-now)
       (check (equal? 3 foo-now))))

   (prof:count (hairy-macro-test foo) 1
     (lambda (foo-now)
       ;; Checking the closure is made in the right environment
       (check (equal? (* datum datum) foo-now))))
   (check (equal? '(0 (data 0 (2 2 (subnote 1)) (3 1) (5 3) (9 1))
		      (bar 2) (foo 4))
		  (show-my-node)))
   (check (equal? '((data (2 2 (subnote 1)) (3 1) (5 3) (9 1))
		    (bar 2) (foo 4))
		  (prof:node-clean-copy my-node)))

   (prof:reset-stats! '(hairy-macro-test data))
   (check (equal? '(0 (data 0 (2 0 (subnote 0)) (3 0) (5 0) (9 0))
		      (bar 2) (foo 4))
		  (show-my-node)))
   (check (equal? '((bar 2) (foo 4)) (prof:node-clean-copy my-node)))

   ;; This is dangerous, because it breaks prof:counts that depend on
   ;; a fixed subitem of (hairy-macro-test data).  But the present
   ;; test doesn't have any of those.
   (prof:clear-stats! '(hairy-macro-test data))
   (check (equal? '(0 (data 0) (bar 2) (foo 4)) (show-my-node)))
   (check (equal? '((bar 2) (foo 4)) (prof:node-clean-copy my-node)))
   )
 )
