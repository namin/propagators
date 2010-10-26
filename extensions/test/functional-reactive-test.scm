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
 functional-reactive

 (define-each-check
   (generic-match #(frp seconds 0) (make-frpremise 'seconds 0)))

 (define-test (glitch)
   (interaction
    (initialize-scheduler)
    (define-cell one)
    (define-cell seconds)
    (define-cell seconds+one)
    (p:+ one seconds seconds+one)
    (add-content one 1)
    (add-content seconds (make-frs 0 (make-frpremise 'seconds 0)))
    (run)
    (content seconds+one)
    (produces #(frs 1 (#(frp seconds 0))))

    (define-cell seconds+one-again)
    (define-cell glitchable)
    (p:< seconds seconds+one-again glitchable)
    (add-content seconds+one-again (content seconds+one))
    (run)
    (content glitchable)
    (produces #(frs #t (#(frp seconds 0))))
   
    (add-content seconds (make-frs 1 (make-frpremise 'seconds 1)))
    (content seconds)
    (produces #(frs 1 (#(frp seconds 1))))
    (run)
    (content seconds+one)
    (produces #(frs 2 (#(frp seconds 1))))
    ;; Rather than glitching, it should notice that its input is out of
    ;; date
    (content glitchable)
    (produces #(frs #t (#(frp seconds 0))))

    ;; But when updated, it should propagate
    (add-content seconds+one-again (content seconds+one))
    (run)
    (content glitchable)
    (produces #(frs #t (#(frp seconds 1))))))
 )
