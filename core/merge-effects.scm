;;; ----------------------------------------------------------------------
;;; Copyright 2010 Massachusetts Institute of Technology.
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

;;; Data structure to represent a merge result that may have effects.

(define-structure effectful
  info
  effects)

(define (effectful-return info)
  (make-effectful info '()))

(define (->effectful thing)
  (if (effectful? thing)
      thing
      (effectful-return thing)))

(define (effectful-> effectful)
  (let ((effectful (remove-redundant-effects effectful)))
    (if (null? (effectful-effects effectful))
	(effectful-info effectful)
	effectful)))

(define (remove-redundant-effects effectful)
  (make-effectful
   (effectful-info effectful)
   (filter (lambda (effect)
	     (not (redundant-effect? effect)))
	   (effectful-effects effectful))))

(define redundant-effect?
  (make-generic-operator 1 'redundant-effect? (lambda (thing) #f)))

(define (effectful-flatten effectful)
  (let ((subeffectful (->effectful (effectful-info effectful))))
    (let ((subinfo (effectful-info subeffectful))
	  (subeffects (effectful-effects subeffectful))
	  (effects (effectful-effects effectful)))
      (make-effectful subinfo (append effects subeffects)))))

(define (effectful-merge e1 e2)
  (let ((e1 (->effectful e1))
	(e2 (->effectful e2)))
    (let ((info-merge (->effectful (merge (effectful-info e1)
					  (effectful-info e2)))))
      (effectful->
       (make-effectful
	(effectful-info info-merge)
	(append (effectful-effects e1)
		(effectful-effects info-merge)
		(effectful-effects e2)))))))

(define (effectful-bind effectful func)
  (let ((effectful (->effectful effectful)))
    (effectful->
     (effectful-flatten
      (make-effectful
       (->effectful (func (effectful-info effectful)))
       (effectful-effects effectful))))))

(define (effectful-list-bind effectfuls func)
  (let ((effectfuls (map ->effectful effectfuls)))
    (effectful->
     (effectful-flatten
      (make-effectful
       (->effectful (func (map effectful-info effectfuls)))
       (apply append (map effectful-effects effectfuls)))))))
