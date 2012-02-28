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
 sudoku

 (define-test (parse-smoke)
   (initialize-scheduler)
   (check
    (equal?
     "????\n????\n????\n?3??\n"
     (with-output-to-string
       (lambda ()
	 (print-sudoku-board
	  (parse-sudoku
	   '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 3 0 0)))))))))

 (define-test (solve-smoke)
   (initialize-scheduler)
   (check
    (equal?
     "3124\n2431\n1243\n4312\n"
     (with-output-to-string
       (lambda ()
	 (do-sudoku '((0 1 2 0)
		      (0 0 0 0)
		      (0 0 4 0)
		      (0 3 0 0)))))))
   (check (= (if *avoid-false-true-flips* 52 49)
	     *number-of-calls-to-fail*)))

 )
