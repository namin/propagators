;;; ----------------------------------------------------------------------
;;; Copyright 2009-2010 Gerald Jay Sussman and Alexey Radul.
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

(initialize-scheduler)

(define n0 (node 2))
(define n0t1 (car n0))
(define n0t2 (cadr n0))

(define n1 (node 2))
(define n1t1 (car n1))
(define n1t2 (cadr n1))

(define n2 (node 2))
(define n2t1 (car n2))
(define n2t2 (cadr n2))

; (ground n0)
(plunker (potential n0t1))

(define-cell Pv ((voltage-source 6) n1t1 n0t1))
(define-cell PR1 ((linear-resistor 4) n1t2 n2t1))
(define-cell PR2 ((linear-resistor 2) n2t2 n0t2))

(define-cell power (e:+ Pv (e:+ PR1 PR2)))

(plunker (potential n2t1))

(run)

(pp (content (potential n2t1)))
#|
#[:symbolic 15]
(expression (+ 2 x43))
(metadata #[:symbolic-metadata 16])
|#

(pec (symbolic-expression (content (current n2t2))))
#| Result:
1
|#

(pec (symbolic-expression (content power)))
#| Result:
0
|#
