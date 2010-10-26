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
 recursive-sqrt

 (define-test (one-heron-step)
   (interaction
    (initialize-scheduler)
    (define-cell x)
    (define-cell guess)
    (define-cell better-guess)

    (p:heron-step x guess better-guess)

    (add-content x 2)
    (add-content guess 1.4)
    (run)
    (content better-guess)
    (produces 1.4142857142857141)
    ))

 (define-test (sqrt)
   (interaction
    (initialize-scheduler)
    (define-cell x)
    (define-cell answer)

    (p:sqrt-network x answer)

    (add-content x 2)
    (run)
    (content answer)
    (produces 1.4142135623746899)
    ))

 (define-test (factorial-1)
   (interaction
    (initialize-scheduler)
    (define-cell n! (e:factorial-1 5))
    (run)
    (content n!)
    (produces 120)))
 
 (define-test (factorial-2)
   (interaction
    (initialize-scheduler)
    (define-cell n! (e:factorial-2 5))
    (run)
    (content n!)
    (produces 120)))

 (define-test (factorial-3)
   (interaction
    (initialize-scheduler)
    (define-cell n! (e:factorial-3 5))
    (run)
    (content n!)
    (produces 120)))
 
 (define-test (factorial-4)
   (interaction
    (initialize-scheduler)
    (define-cell n! (e:factorial-4 5))
    (run)
    (content n!)
    (produces 120)))
 
 (define-test (factorial-5)
   (let-cell-rec (fact (e:kernel fact))
     (let-cell (answer (e@ fact 4))
       (run)
       (check (= 24 (content answer))))))
 )
