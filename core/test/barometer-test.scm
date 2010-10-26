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
 barometer

 (define-test (barometer-example)
   (interaction
    (initialize-scheduler)
    (define-cell barometer-height)
    (define-cell barometer-shadow)
    (define-cell building-height)
    (define-cell building-shadow)
    (c:similar-triangles
     barometer-shadow barometer-height building-shadow building-height)
    (add-content building-shadow (make-interval 54.9 55.1))
    (add-content barometer-height (make-interval 0.3 0.32))
    (add-content barometer-shadow (make-interval 0.36 0.37))
    (run)

    (content building-height)
    (produces #(interval 44.51351351351351 48.977777777777774))

    (define-cell fall-time)
    (c:fall-duration fall-time building-height)
    (add-content fall-time (make-interval 2.9 3.1))
    (run)

    (content building-height)
    (produces #(interval 44.51351351351351 47.24276000000001))
      
    (content barometer-height)
    (produces #(interval .3 .3183938287795994))

    (content fall-time)
    (produces #(interval 3.0091234174691017 3.1))

    (add-content building-height 45)
    (run)

    (content barometer-height)
    (produces #(interval .3 .30327868852459017))

    (content barometer-shadow)
    (produces #(interval .366 .37))

    (content building-shadow)
    (produces #(interval 54.9 55.1))

    (content fall-time)
    (produces #(interval 3.025522031629098 3.0321598338046556)))))
