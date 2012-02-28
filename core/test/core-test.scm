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
 core

 (define-test (temperature1)
   (interaction
    (initialize-scheduler)
    (define-cell f)
    (define-cell c)

    (p:fahrenheit->celsius f c)

    (add-content f 77)
    (run)
    (content c)
    (produces 25)
    ))

 (define-test (temperature2)
   (interaction
    (initialize-scheduler)
    (define-cell f)
    (define-cell c)

    (c:fahrenheit-celsius f c)

    (add-content c 25)
    (run)
    (content f)
    (produces 77)

    (define-cell k)

    (c:celsius-kelvin c k)
    (run)
    (content k)
    (produces 298.15)
    ))

 (define-test (barometer-fall-time)
   (interaction
    (initialize-scheduler)
    (define-cell fall-time)
    (define-cell building-height)
    (c:fall-duration fall-time building-height)

    (add-content fall-time (make-interval 2.9 3.1))
    (run)
    (content building-height)
    (produces #(interval 41.163 47.243))
    ))

 (define-test (barometer)
   (interaction
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
    (produces #(interval 44.514 48.978))

    (define-cell fall-time)
    (c:fall-duration fall-time building-height)

    (add-content fall-time (make-interval 2.9 3.1))
    (run)
    (content building-height)
    (produces #(interval 44.514 47.243))

    (content barometer-height)
    (produces #(interval .3 .31839))
    ;; Refining the (make-interval 0.3 0.32) we put in originally

    (content fall-time)
    (produces #(interval 3.0091 3.1))
    ;; Refining (make-interval 2.9 3.1)

    (add-content building-height (make-interval 45 45))
    (run)
    (content barometer-height)
    (produces #(interval .3 .30328))

    (content barometer-shadow)
    (produces #(interval .366 .37))

    (content building-shadow)
    (produces #(interval 54.9 55.1))

    (content fall-time)
    (produces #(interval 3.0255 3.0322))
    ))

 (define-test (barometer-reverse-fall-time)
   (interaction
    (initialize-scheduler)
    (define-cell fall-time)
    (define-cell building-height)
    (c:fall-duration fall-time building-height)

    (add-content fall-time (make-interval 2.9 3.1))
    (run)
    (content building-height)
    (produces #(interval 41.163 47.243))

    (add-content building-height 45)

    (run)
    (content fall-time)
    (produces #(interval 3.0255 3.0322))
    ))

 (define-each-check
   (= 4 (let-cell (x 4) (run) (content x)))
   (= 5 (let-cells ((x (e:constant 2))
		    (y 3))
	  (let-cell (z (e:+ x y))
	    (run)
	    (content z))))
   (= 7 (let-cells* ((x 3)
		     (y (e:+ x 4)))
	  (run)
	  (content y)))
   (= 1 (let-cell ((answer (ce:+ 2 %% 3)))
	  (run)
	  (content answer)))
   (= 7 (let-cells-rec ((z (e:+ x y))
			(x (e:- z y))
			(y (e:- z x)))
	  (c:id x 4)
	  (c:id y 3)
	  (run)
	  (content z)))
   (= 4 (let-cells-rec ((z (e:+ x y))
			(x (e:- z y))
			(y (e:- z x)))
	  (c:id z 7)
	  (c:id y 3)
	  (run)
	  (content x)))
   (= 3 (let-cells-rec ((z (e:+ x y))
			(x (e:- z y))
			(y (e:- z x)))
	  (c:id x 4)
	  (c:id z 7)
	  (run)
	  (content y))))

 (define-test (serpent)
   (initialize-scheduler)
   (let-cell-rec (ones (e:cons 1 ones))
     (run)
     (check (eqv? 1 (content (e:car ones))))
     (check (eqv? 1 (content (e:car (e:cdr ones)))))
     (check (eqv? 1 (content (e:car (e:cdr (e:cdr ones))))))))

 (define-test (monotonic-intervals)
   (interaction
    (initialize-scheduler)
    (define-cell range1 (make-interval -5 5))
    (define-cell range2 (make-interval -5 5))
    (define-cell less? (e:< range1 range2))
    (define-cell same? (e:= range1 range2))
    (define-cell more? (e:> range1 range2))
    (run)
    (add-content range1 (make-interval 3 5))
    (add-content range2 (make-interval 1 2))
    (run)
    (content less?)
    (produces #f)
    (content same?)
    (produces #f)
    (content more?)
    (produces #t)))

 (define-test (divisible-intervals)
   (check (contradictory? (generic-/ (make-interval 3 4) 0)))
   (check (contradictory? (generic-/ (make-interval 3 4) (make-interval 0 0)))))
 )
