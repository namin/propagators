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

(define-propagator-syntax (p:multiple-dwelling)
  (let-cells (baker cooper fletcher miller smith
		    b=5 c=1 f=5 f=1 m>c sf fc one five s-f as-f f-c af-c)
    (one-of 1 2 3 4 5 baker)       (one-of 1 2 3 4 5 cooper)
    (one-of 1 2 3 4 5 fletcher)    (one-of 1 2 3 4 5 miller)
    (one-of 1 2 3 4 5 smith)
    (require-distinct
     (list baker cooper fletcher miller smith))
    ((constant 1) one)         ((constant 5) five)
    (p:= five baker b=5)       (forbid b=5)
    (p:= one cooper c=1)       (forbid c=1)
    (p:= five fletcher f=5)    (forbid f=5)
    (p:= one fletcher f=1)     (forbid f=1)
    (p:> miller cooper m>c)    (require m>c)
    (p:- smith fletcher s-f)
    (p:abs s-f as-f)
    (p:= one as-f sf)          (forbid sf)
    (p:- fletcher cooper f-c)
    (p:abs f-c af-c)
    (p:= one af-c fc)          (forbid fc)
    (list baker cooper fletcher miller smith)))

#|
 (initialize-scheduler)
 (define answers (p:multiple-dwelling))
 (run)
 (map v&s-value (map tms-query (map content answers)))
 ;Value: '(3 2 4 5 1)

 *number-of-calls-to-fail*
 ;Value: 33
|#

;;; Here's how you write the same program in expression style
(define-propagator-syntax (p:multiple-dwelling-expressions)
  (let-cells ((baker    (e:one-of 1 2 3 4 5))
	      (cooper   (e:one-of 1 2 3 4 5))
	      (fletcher (e:one-of 1 2 3 4 5))
	      (miller   (e:one-of 1 2 3 4 5))
	      (smith    (e:one-of 1 2 3 4 5)))
    (require-distinct
     (list baker cooper fletcher miller smith))
    (forbid (e:= baker 5))
    (forbid (e:= cooper 1))
    (forbid (e:= fletcher 5))
    (forbid (e:= fletcher 1))
    (require (e:> miller cooper))
    (forbid (e:= 1 (e:abs (e:- fletcher smith))))
    (forbid (e:= 1 (e:abs (e:- fletcher cooper))))
    (list baker cooper fletcher miller smith)))

#|
 (initialize-scheduler)
 (define answers (p:multiple-dwelling-expressions))
 (run)
 (map v&s-value (map tms-query (map content answers)))
 ;Value: '(3 2 4 5 1)

 *number-of-calls-to-fail*
 ;Value: 33
|#
