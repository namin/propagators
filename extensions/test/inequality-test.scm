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
 inequalities

 (define (test-try-ineq ineq)
   (try-inequality ineq (lambda (x) x) (lambda () 'failed)))

 (define-each-check
   (tautological-ineq? (make-tautological-ineq))
   (contradictory-ineq? (make-contradictory-ineq))

   (tautological-ineq?
    (test-try-ineq (make-inequality '> '(expt x 2))))
   (contradictory-ineq?
    (test-try-ineq (make-inequality '< '(expt x 2))))

   (generic-match
    (make-solved-inequality '< 'y -3)
    (test-try-ineq (make-inequality '< '(+ y 3))))
   (generic-match
    (make-solved-inequality '< 'y -3/2)
    (test-try-ineq (make-inequality '< '(+ (* 2 y) 3))))
   (generic-match
    (make-solved-inequality '<= 'x 0)
    (test-try-ineq (make-inequality '<= '(* 23 x))))
   (generic-match
    (make-solved-inequality '>= 'x 0)
    (test-try-ineq (make-inequality '<= '(* -23 x))))
   (generic-match
    (make-solved-inequality '< 'x 4/23)
    (test-try-ineq (make-inequality '> '(+ 4 (* -23 x)))))

   (generic-match
    'failed
    (test-try-ineq (make-inequality '> '(+ x y))))
   (generic-match
    'failed
    (test-try-ineq (make-inequality '> '(+ 5 (exp y) 8))))
   (generic-match
    'failed
    (test-try-ineq (make-inequality '> '(+ (expt z 2) z 3))))
   
   (generic-match '() (simplify-inequalities '()))
   (generic-match
    (list (make-inequality '> 'x))
    (simplify-inequalities
     (list (make-inequality '> 'x))))
   (generic-match
    (list (make-solved-inequality '< 'x 5/2))
    (simplify-inequalities
     (list (make-inequality '< '(- x 4))
	   (make-inequality '< '(- (* 2 x) 5)))))
   (generic-match
    (list (make-solved-inequality '< 'y 4)
	  (make-solved-inequality '< 'x 5/2))
    (simplify-inequalities
     (list (make-inequality '< '(- y 4))
	   (make-inequality '< '(- (* 2 x) 5)))))

   (generic-match
    (list (make-solved-inequality '< 'x 4))
    (simplify-inequalities
     (list (make-inequality '< '(- x 4))
	   (make-inequality '<= '(- (* 2 x) 8)))))
   (generic-match
    (list (make-solved-inequality '< 'x 4)
	  (%make-inequality '>= '(+ (expt z 2) z) -2))
    (simplify-inequalities
     (list (make-inequality '< '(- x 4))
	   (make-inequality '>= '(+ z 2 (* z z)))
	   (make-inequality '<= '(- (* 2 x) 8)))))

   (generic-match
    (list (make-solved-inequality '>= 'y 4)
	  (make-solved-inequality '< 'x 5/2))
    (simplify-inequalities
     (list (make-inequality '>= '(- y 4))
	   (make-inequality '< '(- (* 2 x) 5)))))
   (generic-match
    #f
    (simplify-inequalities
     (list (make-inequality '>= '(- x 4))
	   (make-inequality '< '(- (* 2 x) 5)))))
   (generic-match
    #f
    (simplify-inequalities
     (list (make-inequality '>= '(- x 4))
	   (make-inequality '>= '(+ z 2 (* z z)))
	   (make-inequality '< '(- (* 2 x) 5)))))
   (generic-match
    '()
    (simplify-inequalities
     (list (make-inequality '>= '(+ (* 3 x) (* -2 x) (* -1 x) 4)))))
   (generic-match
    #f
    (simplify-inequalities
     (list (make-inequality '>= '(+ (* 3 x) (* -2 x) (* -1 x) 4))
	   (make-inequality '>= '(+ (* 3 y) (* -2 y) (* -1 y) -4)))))
   
   (generic-match
    (make-inequality '< -1)
    (transitive-ineq (make-solved-inequality '> 'x 4)
		     (make-solved-inequality '< 'x 5)))

   (equal?
    '()
    (solve-inequalities '((<= (* x y) (* x y)))))

   (equal?
    '((>= x -2))
    (solve-inequalities '((<= (+ (* 2 x) 1) (+ (* 5 x) 7)))))

   (equal?
    '((>= (* x y) -2))
    (solve-inequalities '((<= (+ (* 2 (* x y)) 1) (+ (* 5 (* x y)) 7)))))

   (equal?
    '((>= (* x y) -2))
    (solve-inequalities '((<= (+ (* 2 (* x y)) 1) (+ (* 5 (* x y)) 7))
			  (>= (* x y) -3))))
   ))
 

