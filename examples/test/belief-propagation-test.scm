;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul.
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
 belief-propagation

 (define-each-check
   (generic-match
    #(message ((#t . 1.) (#f . 1.))) ; Message to Alarm
    (pointwise-sum-product
     (lambda (alarm john)
       (force-assoc
	(list alarm john)
	'(((#t #t) . .9)
	  ((#t #f) . .09999999999999998)
	  ((#f #t) . .05)
	  ((#f #f) . .95))))
     (list #t #f) ; Alarm's support
     (make-message '((#t . 1) (#f . 1))))) ; Message from John
   (generic-match
    #(message ((#t . 1.) (#f . 1.))) ; Message to earthquake
    (pointwise-sum-product
     (lambda (earthquake burglary alarm)
       (force-assoc
	(list burglary earthquake alarm)
	'(((#t #t #t) . .95)
	  ((#t #t #f) . 5.0000000000000044e-2)
	  ((#t #f #t) . .94)
	  ((#t #f #f) . .06000000000000005)
	  ((#f #t #t) . .29)
	  ((#f #t #f) . .71)
	  ((#f #f #t) . .001)
	  ((#f #f #f) . .999))))
     (list #t #f)			       ; Earthquake's support
     (make-message '((#t . .001) (#f . .999))) ; Message from burglary
     (make-message '((#t . 1)    (#f . 1)))))) ; Message from alarm

 (define-test (burglary)
   (interaction
    (initialize-scheduler)
    (define nodes (build-burglary-network))
    (run)
    (map content (map node-marginal nodes))
    (produces
     '(#(message ((#t . 1.6283729946769937e-2) (#f . .98371627005323)))
       #(message ((#t . 1.1394968773811182e-2) (#f . .9886050312261888)))
       #(message ((#t . .04343771179992706) (#f . .9565622882000729)))
       #(message ((#t . 1.) (#f . 0.)))
       #(message ((#t . .03997202114194967) (#f . .9600279788580504)))))))
)
