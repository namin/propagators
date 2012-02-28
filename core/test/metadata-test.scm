;;; ----------------------------------------------------------------------
;;; Copyright 2011 Alexey Radul and Gerald Jay Sussman
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
 metadata

 (define-test (macrology-smoke)
   (initialize-scheduler)
   (let-cells ((foo (make-cell))
	       bar
	       (baz (make-cell)))
     (check (eq? 'foo (name foo)))
     (check (not (eq-get foo 'name)))
     (check (eq? 'bar (name bar)))
     (check (eq? 'bar (eq-get bar 'name)))
     (check (eq? 'baz (name baz)))
     (check (not (eq-get baz 'name)))
     ))
)
