;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul
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

(define (coercability-tester name)
  (make-generic-operator 1 name (lambda (x) #f)))

(define (coercer name #!optional operation)
  (make-generic-operator 1 name operation))

(define (tag-coercion-metadata predicate coercer tester)
  (eq-put! coercer 'coercability-tester tester)
  (eq-put! coercer 'predicate predicate))

(define (declare-coercion type coercer #!optional coercion)
  (let ((the-tester (eq-get coercer 'coercability-tester)))
    (if the-tester
	(defhandler the-tester (lambda (thing) #t) type)
	(error "No tester available for" coercer)))
  (if (not (default-object? coercion))
      (defhandler coercer coercion type)))

(define-syntax declare-named-coercion-target
  (syntax-rules ()
    ((_ predicate-name coercability-name coercer-name operation)
     (begin
       (define coercability-name
	 (coercability-tester 'coercability-name))
       (define coercer-name
	 (coercer 'coercer-name operation))
       (defhandler coercer-name (lambda (x) x) predicate-name)
       (tag-coercion-metadata predicate-name coercer-name coercability-name)))
    ((_ predicate-name coercability-name coercer-name)
     (declare-named-coercion-target
      predicate-name coercability-name coercer-name #!default))))

(define-syntax declare-coercion-target
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((name (cadr form))
	   (opt-operation (cddr form)))
       (let ((pred-name (symbol name '?))
	     (coercability-name (symbol name '-able?))
	     (coercer-name (symbol '-> name)))
	 `(declare-named-coercion-target
	   ,pred-name ,coercability-name, coercer-name ,@opt-operation))))))

(define (defhandler-coercing operation handler coercer)
  (let ((predicate (eq-get coercer 'predicate))
	(coercability-tester (eq-get coercer 'coercability-tester)))
    (defhandler operation handler predicate predicate)
    (defhandler operation
      (coercing coercer handler) predicate coercability-tester)
    (defhandler operation
      (coercing coercer handler) coercability-tester predicate)))
