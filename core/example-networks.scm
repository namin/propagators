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

;;; Example usages of propagator networks

;;; Unidirectional Fahrenheit to Celsius conversion

(define-e:propagator (e:fahrenheit->celsius f)
  (e:* (e:- f 32) 5/9))

#|
 (initialize-scheduler)
 (define-cell f)
 (define-cell c)

 (p:fahrenheit->celsius f c)

 (add-content f 77)
 (run)
 (content c)
 ;Value: 25
|#

#|
 ;;; Here is a much more explicit way to write the same program

 (define-propagator (fahrenheit->celsius f c)
   (let-cells (thirty-two f-32 five c*9 nine)
     ((constant 32) thirty-two)
     ((constant 5) five)
     ((constant 9) nine)
     (p:- f thirty-two f-32)
     (p:* f-32 five c*9)
     (p:/ c*9 nine c)))
|#

;;; Multidirectional Fahrenheit to Celsius to Kelvin conversion

(define-propagator (c:fahrenheit-celsius f c)
  (c:== (ce:+ (ce:* c 9/5) 32) f))
(define-propagator (c:celsius-kelvin c k)
  (c:+ c 273.15 k))

#|
 (initialize-scheduler)
 (define-cell f)
 (define-cell c)

 (c:fahrenheit-celsius f c)

 (add-content c 25)
 (run)
 (content f)
 ;Value: 77

 (define-cell k)

 (c:celsius-kelvin c k)
 (run)
 (content k)
 ;Value: 298.15
|#

#|
 ;;; Same as above, but in diagram style

 (define-propagator (fahrenheit-celsius f c)
   (let-cells (f-32 c*9)
     (c:+ 32 f-32 f)
     (c:* f-32 5 c*9)
     (c:* c 9 c*9)))
|#

;;; Measuring the height of a building using a barometer

(define-e:propagator (ce:fall-duration t)
  (let-cell (g (make-interval 9.789 9.832))
    (ce:* 1/2 (ce:* g (ce:square t)))))

#|
 (initialize-scheduler)
 (define-cell fall-time)
 (define-cell building-height)
 (c:fall-duration fall-time building-height)

 (add-content fall-time (make-interval 2.9 3.1))
 (run)
 (content building-height)
 ;Value: #(interval 41.163 47.243)
|#
;;; In more ways than one

(define-propagator (c:similar-triangles s-ba h-ba s h)
  (c:== (ce:* s-ba %% h-ba)
	(ce:* s %% h)))
#|
 (initialize-scheduler)
 (define-cell barometer-height)
 (define-cell barometer-shadow)
 (define-cell building-height)
 (define-cell building-shadow)
 (c:similar-triangles barometer-shadow barometer-height
		      building-shadow building-height)

 (add-content building-shadow (make-interval 54.9 55.1))
 (add-content barometer-height (make-interval 0.3 0.32))
 (add-content barometer-shadow (make-interval 0.36 0.37))
 (run)
 (content building-height)
 ;Value: #(interval 44.514 48.978)

 (define-cell fall-time)
 (c:fall-duration fall-time building-height)

 (add-content fall-time (make-interval 2.9 3.1))
 (run)
 (content building-height)
 ;Value: #(interval 44.514 47.243)

 (content barometer-height)
 ;Value: #(interval .3 .31839)
 ;; Refining the (make-interval 0.3 0.32) we put in originally

 (content fall-time)
 ;Value: #(interval 3.0091 3.1)
 ;; Refining (make-interval 2.9 3.1)

 (add-content building-height (make-interval 45 45))
 (run)
 (content barometer-height)
 ;Value: #(interval .3 .30328)

 (content barometer-shadow)
 ;Value: #(interval .366 .37)

 (content building-shadow)
 ;Value: #(interval 54.9 55.1)

 (content fall-time)
 ;Value: #(interval 3.0255 3.0322)
|#

;;; More goodies in ../examples/*
