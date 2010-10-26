;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul and Gerald Jay Sussman
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
 carrying-cells
 (define-test (smoke)
   (interaction
    (initialize-scheduler)
    (define-cell bill (make-tms (supported 3 '(bill))))
    (define-cell bill-cons (e:carry-cons nothing bill))
    (define-cell answer)
    (c:== bill-cons answer)
    (define-cell fred (make-tms (supported 4 '(fred))))
    (define-cell fred-cons (e:carry-cons fred nothing))
    (define-cell george (make-tms (supported #t '(george))))
    (conditional-wire george fred-cons answer)
    (define-cell the-pair? (e:carry-pair? answer))
    (define-cell the-car (e:carry-car answer))
    (define-cell the-cdr (e:carry-cdr answer))
    (run)
    ; (pp (content answer))
    (content the-pair?)
    (produces #t)
    (content the-car)
    (produces #(tms (#(supported 4 (fred george)))))
    (content the-cdr)
    (produces #(tms (#(supported 3 (bill)))))))

 (define-test (early-access-test)
   (interaction
    (initialize-scheduler)
    (define-cell source-car)
    (define-cell source-cdr)
    (define-cell the-pair (e:carry-cons source-car source-cdr))
    (check (eq? source-car (e:carry-car the-pair)))
    (check (eq? source-cdr (e:carry-cdr the-pair)))
    ))

 (define-test (deposit)
   (interaction
    (initialize-scheduler)
    (define-cell two-cell (e:deposit 2))
    (run)
    (check (cell? two-cell))
    (check (cell? (content two-cell)))
    (content (content two-cell))
    (produces 2)
    (define-cell examined (e:examine two-cell))
    (content examined)
    (produces 2)))

 (define-test (examine)
   (interaction
    (initialize-scheduler)
    (define-cell examinee)
    (define-cell exam (e:examine examinee))
    (add-content exam 2)
    (run)
    (check (cell? (content examinee)))
    (content (content examinee))
    (produces 2)))
 )
