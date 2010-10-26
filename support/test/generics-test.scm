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

(in-test-group
 generics

 (define-test (smoke)
   (define smoke-g (make-generic-operator-axch 1 'smoke))
   (check (generic-procedure? (get-operator-record smoke-g)))
   (check (= 1 (generic-procedure-arity (get-operator-record smoke-g))))
   (defhandler-axch smoke-g (lambda (x) 'integer) <integer>)
   (check (eq? 'integer (smoke-g 2)))
   (defhandler-axch smoke-g (lambda (x) 'symbol) symbol?)
   (check (eq? 'symbol (smoke-g 'foo)))
   (check (eq? 'integer (smoke-g 2))))

 (define-test (binary-smoke)
   (define smoke-g (make-generic-operator-axch 2 'smoke))
   (check (= 2 (generic-procedure-arity (get-operator-record smoke-g))))
   (defhandler-axch smoke-g (lambda (x y) '(integer integer)) <integer> <integer>)
   (check (equal? '(integer integer) (smoke-g 2 2)))
   (defhandler-axch smoke-g (lambda (x y) '(integer symbol)) <integer> symbol?)
   (defhandler-axch smoke-g (lambda (x y) '(symbol integer)) symbol? <integer>)
   (defhandler-axch smoke-g (lambda (x y) '(symbol symbol)) symbol? symbol?)
   (check (equal? '(integer integer) (smoke-g 2 2)))
   (check (equal? '(integer symbol)  (smoke-g 2 'foo)))
   (check (equal? '(symbol integer)  (smoke-g 'foo 2)))
   (check (equal? '(symbol symbol)   (smoke-g 'foo 'foo)))

   (defhandler-axch smoke-g (lambda (x y) '(integer vector)) <integer> vector?)
   (defhandler-axch smoke-g (lambda (x y) '(vector integer)) vector? <integer>)
   (defhandler-axch smoke-g (lambda (x y) '(vector vector)) vector? vector?)
   (check (equal? '(integer vector)  (smoke-g 2 #())))
   (check (equal? '(vector integer)  (smoke-g #() 2)))
   (check (equal? '(vector vector)  (smoke-g #() #())))

   (check (equal? '(integer integer) (smoke-g 2 2)))
   (check (equal? '(integer symbol)  (smoke-g 2 'foo)))
   (check (equal? '(symbol integer)  (smoke-g 'foo 2)))
   (check (equal? '(symbol symbol)   (smoke-g 'foo 'foo))))

 (define-test (guarded)
   (define test-g (make-generic-operator-axch 1 'test))
   (defhandler-axch test-g (lambda (x) 'even) <integer>)
   (defhandler-axch test-g (lambda (x) 'odd) (guard <integer> odd?))
   (check (eq? 'even (test-g 2)))
   (check (eq? 'odd (test-g 3)))

   (defhandler-axch test-g (lambda (x) 'exact) (guard <number> exact?))
   (check (eq? 'even (test-g 2)))
   (check (eq? 'odd (test-g 3)))
   (check (eq? 'exact (test-g 3/2))))
 
 (define-test (explicit-guards)
   (define test-g (make-generic-operator-axch 1 'test))
   (defhandler-axch test-g (lambda (x) 'object) <object>)
   (define (bogus? x)
     (or (vector? x)
	 (and (string? x)
	      (< (string-length x) 2))))

   (defhandler-axch test-g (lambda (x) 'bogus) bogus?)
   (check (eq? 'bogus (test-g #())))
   (check (eq? 'bogus (test-g "")))
   (check (eq? 'object (test-g 1)))

   ;; Yes, this is wrong; I'm trying to test overriding
   (declare-explicit-guard bogus? (guard <integer> any?))
   (defhandler-axch test-g (lambda (x) 'guard-bogus) bogus?)
   (check (eq? 'bogus (test-g #())))
   (check (eq? 'bogus (test-g "")))
   (check (eq? 'guard-bogus (test-g 1)))

   (declare-explicit-guard bogus? <string>)
   (defhandler-axch test-g (lambda (x) 'type-guard-bogus) bogus?)
   (check (eq? 'bogus (test-g #())))
   (check (eq? 'type-guard-bogus (test-g "")))
   (check (eq? 'object (test-g "abc")))
   )

 (define-test (inspection)
   (define test-g (make-generic-operator-axch 1 'test))
   (define (say-object x) 'object)
   (define (say-bogus x) 'bogus)
   (define (say-guard-bogus x) 'guard-bogus)
   (define (say-type-guard-bogus x) 'type-guard-bogus)
   (defhandler-axch test-g say-object <object>)
   (define (bogus? x)
     (or (vector? x)
	 (and (string? x)
	      (< (string-length x) 2))))

   (defhandler-axch test-g say-bogus bogus?)

   ;; Yes, this is wrong; I'm trying to test overriding
   (declare-explicit-guard bogus? (guard <integer> any?))
   (defhandler-axch test-g say-guard-bogus bogus?)

   (declare-explicit-guard bogus? <string>)
   (defhandler-axch test-g say-type-guard-bogus bogus?)
   (check (eq? say-bogus (selected-handler test-g '(#()))))
   (check (eq? say-type-guard-bogus (selected-handler test-g '(""))))
   (check (eq? say-object (selected-handler test-g '("abc"))))
   (check (equal?
	   `(((,<string>)
	      (,bogus? . ,say-type-guard-bogus))
	     ((,<object>)
	      (,bogus? . ,say-bogus)
	      (,any? . ,say-object)))
	   (handler-search-trees test-g '(""))))
   (check (equal?
	   `(((,<integer>)
	      (,any? . ,say-guard-bogus))
	     ((,<object>)
	      (,bogus? . ,say-bogus)
	      (,any? . ,say-object)))
	   (handler-search-trees test-g '(1))))
   ))

