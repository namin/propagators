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

(define-method generic-match ((pattern <vector>) (object rtd:effectful))
  (generic-match
   pattern
   (vector 'effectful (effectful-info object)
	   (effectful-effects object))))

;;; Test slotful structure

(define-structure (kons (constructor kons))
  kar
  kdr)
(declare-type-tester kons? rtd:kons)

(slotful-information-type kons? kons kons-kar kons-kdr)

(define-method generic-match ((pattern <vector>) (object rtd:kons))
  (generic-match
   pattern
   (vector 'kons (kons-kar object) (kons-kdr object))))

(define-method generic-match ((pattern <vector>) (object rtd:%interval))
  (generic-match
   pattern
   (vector 'interval (interval-low object) (interval-high object))))

(define-method generic-match ((pattern <vector>) (object rtd:nogood-effect))
  (generic-match
   pattern
   (vector 'nogood-effect (nogood-effect-nogood object))))
