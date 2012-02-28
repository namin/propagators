;;; ----------------------------------------------------------------------
;;; Copyright 2009 Massachusetts Institute of Technology.
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

(define (tms-merge tms1 tms2)
  (let ((candidate (tms-assimilate tms1 tms2)))
    (effectful-bind (strongest-consequence candidate)
      (lambda (consequence)
	(if (not (contradictory? consequence))  ; **
	    (tms-assimilate candidate consequence)
	    (make-effectful
	     (tms-assimilate candidate consequence)
	     (list (make-nogood-effect
		    (v&s-support consequence)))))))))

;;; TODO TMS-QUERY is still hopelessly broken.  The problem is that
;;; the effect of signaling a contradiction is being deferred from the
;;; point at which the worldview changes to the point at which some
;;; propagator tries to get the result.


(define (tms-query tms)
  (let ((answer (strongest-consequence tms)))
    (let ((better-tms (tms-assimilate tms answer)))
      (if (not (eq? tms better-tms))
          (set-tms-values! tms (tms-values better-tms)))
      (check-consistent! answer)        ; **
      answer)))

(define (check-consistent! v&s)
  (if (contradictory? v&s)
      (process-nogood! (v&s-support v&s))))

#|
;;; Sussman's tentative and unpleasant patch for Micah's bug.
;;;  Required change to core/test/dependencies-test.scm.

(define (tms-query tms)
  (let ((answer (strongest-consequence tms)))
    (let ((better-tms (tms-assimilate tms answer)))
      (if (not (eq? tms better-tms))
          (set-tms-values! tms (tms-values better-tms)))
      (if (contradictory? answer)
	  (begin (process-nogood! (v&s-support answer))
		 nothing)
	  answer))))
|#

;; Will be replaced by process-nogood! in search.scm
(define (process-nogood! nogood)
  (abort-process `(contradiction ,nogood)))

(define-structure nogood-effect
  nogood)

(defhandler execute-effect
  (lambda (nogood-effect)
    (if (all-premises-in? (nogood-effect-nogood nogood-effect))
	(process-nogood! (nogood-effect-nogood nogood-effect))))
  nogood-effect?)

(defhandler generic-attach-premises
  (lambda (effect)
    (lambda (support)
      (make-nogood-effect
       (lset-union eq? (nogood-effect-nogood effect) support))))
  nogood-effect?)
