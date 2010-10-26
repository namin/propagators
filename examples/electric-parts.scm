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

(declare (usual-integrations make-cell cell?))

;;; Primitive data structures

(define (electric-terminal i e)
  (cons i e))
(define (current terminal)
  (car terminal))
(define (potential terminal)
  (cdr terminal))

;; TODO (re)insert optional names for cells and devices
(define ((2-terminal-device vic) t1 t2)
  "We measure the voltage from t1 to t2 (i.e. v = e(t1) - e(t2)),
and the current is measured as flowing into t1."
  (let ((i1 (current t1)) (e1 (potential t1))
	(i2 (current t2)) (e2 (potential t2)))
    (let-cells (v P)
      (c:+ v e2 e1)
      (c:+ i1 i2 0)
      (c:* i1 v P)
      (vic v i1)
      P)))

(define (linear-resistor R)
  (2-terminal-device
   (lambda (v i)
     (c:* R i v))))

(define (voltage-source strength)
  (2-terminal-device
   (lambda (v i)
     (c:== strength v))))

(define (current-source strength)
  (2-terminal-device
   (lambda (v i)
     (c:== strength i))))

(define (ground node)
  (p:== 0 (potential (car node))))

(define spst-switch p:conditional-wire)

(define (node n)
  (let ((e (make-cell))
	(is
	 (let lp ((n n))
	   (cond ((= n 1)
		  (let-cell i
		    (list i i)))
		 ((= n 2)
		  (let-cells (i1 i2 i)
		    (c:+ i1 i2 i)
		    (list i i1 i2)))
 		 ((even? n)
		  (let ((a1 (lp (/ n 2)))
			(a2 (lp (/ n 2)))
			(a (make-cell)))
		    (c:+ (car a1) (car a2) a)
		    (cons a (append (cdr a1) (cdr a2)))))
		 ((odd? n)
		  (let ((a1 (lp (- n 1)))
			(i2 (make-cell))
			(a (make-cell)))
		    (c:+ (car a1) i2 a)
		    (cons a (cons i2 (cdr a1)))))
		 (else (error))))))
    ((constant 0) (car is))
    (map (lambda (i)
	   (electric-terminal i e))
	 (cdr is))))


;;; Support for slices -- GJS

(define (clone-terminal terminal)
  (electric-terminal (current terminal)
		     (potential terminal)))

(define (ideal-diode)
  (2-terminal-device
   (lambda (v i)
     (let-cells (if>=0 vreverse vr<=0 iforward conducting -conducting)
       ;;#t=>conducting; #f=>not conducting
       (p:amb conducting)
       (p:not conducting -conducting)
       (spst-switch conducting 0 v)
       (spst-switch -conducting v vreverse)
       (c:<= vr<=0 vreverse 0)
       (require vr<=0)
       (spst-switch -conducting 0 i)
       (spst-switch conducting i iforward)
       (c:>= if>=0 iforward 0)
       (require if>=0)))))
