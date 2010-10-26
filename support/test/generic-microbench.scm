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

(declare (usual-integrations))

(load-option 'sos)

(define (repeat count thunk)
  (let loop ((count count))
    (if (<= count 0)
	'ok
	(begin
	  (thunk)
	  (loop (- count 1))))))

;; This version is a thunk combinator!
(define ((repeated count thunk))
  (repeat count thunk))

(define-structure frob foo)
(define-structure blob foo)
(define-structure flob foo)
(define-structure frob2 foo)
(define-structure blob2 foo)
(define-structure flob2 foo)

(define (test thing)
  (or (frob? thing)
      (blob? thing)
      (flob? thing)
      (frob2? thing)
      (blob2? thing)
      (flob2? thing)
      (pair? thing)
      (vector? thing)
      (string? thing)
      (record? thing)
      (procedure? thing)
      (char? thing)
      (boolean? thing)))

(define-generic g-test (thing))

(define-method g-test ((thing <object>))
  #f)

(define-method g-test ((thing rtd:frob))
  #t)

(define-method g-test ((thing rtd:blob))
  #t)

(define-method g-test ((thing rtd:flob))
  #t)

(define-method g-test ((thing rtd:frob2))
  #t)

(define-method g-test ((thing rtd:blob2))
  #t)

(define-method g-test ((thing rtd:flob2))
  #t)

(define-method g-test ((thing <pair>))
  #t)

(define-method g-test ((thing <vector>))
  #t)

(define-method g-test ((thing <string>))
  #t)

(define-method g-test ((thing <record>))
  #t)

(define-method g-test ((thing <procedure>))
  #t)

(define-method g-test ((thing <char>))
  #t)

(define-method g-test ((thing <boolean>))
  #t)

(define gerry-test (make-generic-operator 1 'test (lambda (thing) #f)))
(for-each (lambda (pred)
	    (defhandler gerry-test (lambda (thing) #t) pred))
	  (reverse (list frob? blob? flob? frob2? blob2? flob2?
			 pair? vector? string? record? procedure?
			 char? boolean?)))

(define *flat-types-list* '())

(define (flat? thing)
  (apply boolean/or (map (lambda (type) (type thing)) *flat-types-list*)))

(define (specify-flat type)
  (if (memq type *flat-types-list*)
      'ok
      (set! *flat-types-list* (cons type *flat-types-list*))))

(for-each specify-flat (reverse (list frob? blob? flob? frob2? blob2? flob2?
				      pair? vector? string? record? procedure?
				      char? boolean?)))

(define axch-test-1 (make-generic-operator-axch 1 'test))
(defhandler-axch axch-test-1 (lambda (thing) #f) any?)
(for-each (lambda (pred)
	    (defhandler-axch axch-test-1 (lambda (thing) #t) pred))
	  (reverse (list frob? blob? flob? frob2? blob2? flob2?
			 pair? vector? string? record? procedure?
			 char? boolean?)))

(define axch-test-2 (make-generic-operator-axch 1 'test))
(defhandler-axch axch-test-2 (lambda (thing) #f) <object>)
(for-each (lambda (class)
	    (defhandler-axch axch-test-2 (lambda (thing) #t) class))
	  (reverse (list rtd:frob rtd:blob rtd:flob rtd:frob2 rtd:blob2 rtd:flob2
			 <pair> <vector> <string> <record> <procedure>
			 <char> <boolean>)))

(define axch-test-3 (make-generic-operator-axch 1 'test))
(defhandler-axch axch-test-3 (lambda (thing) #f) <object>)
(for-each (lambda (class)
	    (defhandler-axch axch-test-3 (lambda (thing) #t) class))
	  (reverse (list rtd:frob blob? rtd:flob frob2? rtd:blob2 rtd:flob2
			 <pair> vector? <string> record? <procedure>
			 <char> <boolean>)))


(let ((lst '(,(current-input-port) (1) #(1) "1" () ,(lambda () 1) #f #t ,(make-frob2 1))))
  (let ((answer (map test lst)))
    (for-each
     (lambda (name func)
       (newline)
       (pp `(,name ,(equal? answer (map func lst))))
       (show-time
	(if #t
	    (lambda ()
	      (with-profiling 10
			      (repeated 2000000 (lambda () (map func lst)))))
	    (repeated 2000000 (lambda () (map func lst))))))
     '(test g-test gerry-test #;flat? axch-test-1 axch-test-2 axch-test-3)
     (list test g-test gerry-test #;flat? axch-test-1 axch-test-2 axch-test-3))))
