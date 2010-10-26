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
 symbolics
 
 (define-each-check
   (generic-match
    #(symbolic x #(metadata (x) () ()))
    (variable->symbolic 'x))

   (generic-match
    #(symbolic -1 #(metadata (x) (((= x -1) ())) ()))
    (merge (make-symbolic 'x (make-symbolic-metadata '(x) '() '()))
	   (make-symbolic '(+ (* 2 x) 1) (make-symbolic-metadata '(x) '() '()))))

   (generic-match
    #(symbolic -11
      #(metadata (x z y) (((= z -12) ()) ((= x -1) ()) ((= y -4) ())) ()))
    (merge (make-symbolic '(+ (* 2 x) 3 z)
	    (make-symbolic-metadata '(x z) '(((= y (* 4 x)) ())) '()))
	   (make-symbolic '(- y 7)
	    (make-symbolic-metadata '(y) '(((= x (+ 3 y)) ())) '()))))

   (generic-match
    #(symbolic x #(metadata (x) () ()))
    (merge (make-symbolic 'x (make-symbolic-metadata '(x) '() '()))
	   (make-symbolic 'x (make-symbolic-metadata '(x) '() '()))))

   (equal?
    nothing
    ((nary-unpacking +) (make-symbolic 'x (empty-metadata)) nothing))))
