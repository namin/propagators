;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul and Gerald Jay Sussman
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

(define (information-assq key alist)
  (let ((binding (assq key alist)))
    (if binding
	(cdr binding)
	nothing)))

(define (same-alist? alist1 alist2)
  (lset= (lambda (pair1 pair2)
	   (and (eq? (car pair1) (car pair2))
		(equivalent? (cdr pair1) (cdr pair2))))
	 alist1 alist2))

(define (unary-alist-unpacking f)
  (lambda (alist)
    (map (lambda (binding)
	   (cons (car binding) (f (cdr binding))))
	 alist)))

(define (binary-alist-unpacking f)
  (lambda (alist1 alist2)
    (let ((keys (lset-union eq? (map car alist1) (map car alist2))))
      (define get information-assq)
      (map (lambda (key)
	     (cons key (f (get key alist1) (get key alist2))))
	   keys))))

(define %merge-alist (binary-alist-unpacking merge))

(define (merge-alist alist1 alist2)
  (let ((putative-answer (%merge-alist alist1 alist2)))
    (effectful-list-bind (map cdr putative-answer)
      (lambda (cdrs)
	(map cons
	     (map car putative-answer)
	     cdrs)))))
