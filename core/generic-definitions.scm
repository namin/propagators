;;; ----------------------------------------------------------------------
;;; Copyright 2009 Massachusetts Institute of Technology.
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

;;;; General generic applicative functor machinery

;;; If a group of partial information structures fit into the
;;; applicative functor (TODO: Reference Paterson and McBride)
;;; paradigm, the network can be mechanically extended to handle them
;;; and their compositions.

(define (binary-mapping f)
  (define (loop x y)
    (let ((mapper (binary-map x y)))
      (if (procedure? mapper)
	  (mapper loop)
	  (f x y))))
  (name! loop f)
  loop)

(define binary-map
  (make-generic-operator 2 'binary-map
    (lambda (x y) 'done!)))

(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  nothing? any?)

(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  any? nothing?)


(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  contradictory? any?)

(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  any? contradictory?)

(define (unary-mapping f)
  (name!
   (lambda (x)
     ((binary-mapping (lambda (x y) (f x)))
      ;; TODO Make this 1 a real "object that can be coerced into anything"
      x 1))
   f))

(define (nary-mapping f)
  (name!
   (lambda args
     (case (length args)
       ((0) (f))
       ((1) ((unary-mapping f) (car args)))
       ((2) ((binary-mapping f) (car args) (cadr args)))
       (else
	(let loop ((args '()) (rest args))
	  (if (null? (cdr rest))
	      ((binary-mapping (lambda (lst item)
				 (apply f (reverse (cons item lst)))))
	       args (car rest))
	      (loop ((binary-mapping cons) (car rest) args) (cdr rest)))))))
   f))

;;;; General generic-monadic machinery

;;; If a partial information structure fits into the monad paradigm,
;;; the portions of the network that are necessarily monadic rather
;;; than applicative-functorial can be automatically extended to that
;;; structure.  Of course, since monads do not compose naturally, it
;;; is up to the user to effectively treat a group of partial
;;; information structures as forming a single monad where
;;; appropriate, and define corresponding cross-structure methods for
;;; these operations.
;;; TODO Does anything other than IF really need monads?

(define (generic-bind thing function)
  (generic-flatten (generic-unpack thing function)))

(define generic-unpack 
  (make-generic-operator 2 'unpack
    (lambda (object function)
      (function object))))

(define generic-flatten
  (make-generic-operator 1 'flatten (lambda (object) object)))

(define (%nary-unpacking function)
  (lambda args
    (let loop ((args args)
               (function function))
      (if (null? args)
          (function)
          (generic-bind
           (car args)
           (lambda (arg)
             (loop (cdr args)
                   (lambda remaining-args
                     (apply function (cons arg remaining-args))))))))))

;; This version also attaches the name information, for debugging and
;; drawing networks.
(define (nary-unpacking function)
  (eq-label! (%nary-unpacking function) 'name function))

(defhandler generic-unpack
  (lambda (object function) nothing)
  nothing? any?)

(defhandler generic-unpack
  (lambda (object function) nothing)
  contradictory? any?)


;;; This handler is redundant but harmless
(defhandler generic-flatten
  (lambda (thing) nothing)
  nothing?)
