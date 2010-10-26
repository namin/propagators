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

(declare (usual-integrations make-cell cell?))

(define-e:propagator (e:heron-step x g)
  (e:/ (e:+ g (e:/ x g)) 2))

(define-e:propagator (e:sqrt-iter x g)
  (e:if (x g) (e:good-enuf? x g)
	g
	(e:sqrt-iter x (e:heron-step x g))))

(define-e:propagator (e:sqrt-network x)
  (e:sqrt-iter x 1.0))

(define-e:propagator (e:good-enuf? x g)
  (let-cell (eps .00000001)
    (e:< (e:abs (e:- x (e:* g g))) eps)))

#|
 (initialize-scheduler)
 (define-cell x)
 (define-cell answer (e:sqrt-network x))

 (add-content x 2)
 (run)
 (content answer)
 ;Value: 1.4142135623746899
|#

(define-propagator (p:factorial-1 n n!)
  (p:when (n n!) (e:not (e:= 0 n))
   (p:== (e:* n (e:factorial-1 (e:- n 1))) n!))
  (switch (e:= 0 n) 1 n!))

(define-propagator (p:factorial-2 n n!)
  (p:if (n n!) (e:= 0 n)
   (p:== 1 n!)
   (p:== (e:* n (e:factorial-2 (e:- n 1))) n!)))

(define-e:propagator (e:factorial-3 n)
  (ce:== (e:when (n) (e:not (e:= 0 n))
	   (e:* n (e:factorial-3 (e:- n 1))))
	 (e:switch (e:= 0 n) 1)))

(define-e:propagator (e:factorial-4 n)
  (e:if (n) (e:= 0 n)
	1
	(e:* n (e:factorial-4 (e:- n 1)))))

(define-e:propagator (e:kernel fact)
  (lambda-e:propagator (n)
    (import fact)
    (e:if (n) (e:= n 0)
	  1
	  (e:* n (e@ fact (e:- n 1))))))

