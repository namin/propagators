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
 multiple-dwelling

 (define-test (multiple-dwelling)
   (interaction
    (initialize-scheduler)
    (define answers (p:multiple-dwelling))
    (run)
    (map v&s-value (map tms-query (map content answers)))
    (produces '(3 2 4 5 1))

    *number-of-calls-to-fail*
    (produces (if *false-premise-starts-out*
		  (if *avoid-false-true-flips* 33 51) 63))
    )))
