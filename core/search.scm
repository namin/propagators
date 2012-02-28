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

(define *false-premise-starts-out* #t)
(define *avoid-false-true-flips* #f)

(define (binary-amb cell)
  (let ((true-premise (make-hypothetical 'true cell))
        (false-premise (make-hypothetical 'false cell)))
    (define (amb-choose)
      (if (and *avoid-false-true-flips*
	       (or (premise-in? true-premise)
		   (premise-in? false-premise)))
	  'ok ; the some-premise-is-in invariant holds
	  (let ((reasons-against-true
		 (filter (lambda (nogood)
			   (and (all-premises-in? nogood)
				(not (member false-premise nogood))))
			 (premise-nogoods true-premise)))
		(reasons-against-false
		 (filter (lambda (nogood)
			   (and (all-premises-in? nogood)
				(not (member true-premise nogood))))
			 (premise-nogoods false-premise))))
	    (cond ((null? reasons-against-true)
		   (if *contradiction-wallp* 
		       (pp `(asserting-true ,true-premise
					    ,false-premise
					    ,cell)))
		   (kick-out! false-premise)
		   (bring-in! true-premise))
		  ((null? reasons-against-false)
		   (if *contradiction-wallp* 
		       (pp `(asserting-false ,true-premise
					     ,false-premise
					     ,cell)))
		   (kick-out! true-premise)
		   (bring-in! false-premise))
		  (else			; this amb must fail.
		   (if *contradiction-wallp* 
		       (pp `(amb-fail ,true-premise ,false-premise ,cell)))
		   (kick-out! true-premise)
		   (kick-out! false-premise)
		   (process-contradictions
		    (pairwise-resolve reasons-against-true
				      reasons-against-false)))))))

    (name! amb-choose 'amb-choose)
    ;; This only affects run order, and only in some experimental
    ;; schedulers
    (tag-slow! amb-choose)
    (if *false-premise-starts-out*
	;; Let's have the false premise start unbelieved.
	(mark-premise-out! false-premise))
    
    ;; The cell is a spiritual neighbor...
    (propagator cell amb-choose)

    (let ((diagram
	   (make-anonymous-i/o-diagram amb-choose '() (list cell))))
      ((constant (make-tms
		  (list (supported #t (list true-premise) (list diagram))
			(supported #f (list false-premise) (list diagram)))))
       cell)
      (register-diagram diagram)
      diagram)))

(define (pairwise-resolve nogoods1 nogoods2)
  (append-map (lambda (nogood1)
                (map (lambda (nogood2)
                       (lset-union eq? nogood1 nogood2))
                     nogoods2))
              nogoods1))

(define (process-contradictions nogoods)
  (process-one-contradiction
   (car (sort-by nogoods
          (lambda (nogood)
            (length (filter hypothetical? nogood)))))))

(define (process-one-contradiction nogood)
  (if *contradiction-wallp* (pp `(nogood ,@nogood)))
  (let ((hyps (filter hypothetical? nogood)))
    (if (null? hyps)
	(begin
	  (if *contradiction-wallp* (pp 'nogood-aborted))
	  (abort-process `(contradiction ,nogood)))
        (begin
	  (if *contradiction-wallp*
	      (pp `(kicking-out ,(car hyps))))
          (kick-out! (car hyps))
          (for-each (lambda (premise)
                      (assimilate-nogood! premise nogood))
                    nogood)))))

(define (assimilate-nogood! premise new-nogood)
  (let ((item (delq premise new-nogood))
        (set (premise-nogoods premise)))
    (if (any (lambda (old) (lset<= eq? old item)) set)
        #f
        (let ((subsumed
               (filter (lambda (old) (lset<= eq? item old))
                       set)))
          (set-premise-nogoods! premise
            (lset-adjoin eq?
              (lset-difference eq? set subsumed) item))))))

(define *number-of-calls-to-fail* 0)

(define initialize-scheduler
  (let ((initialize-scheduler initialize-scheduler))
    (lambda ()
      (initialize-scheduler)
      (set! *number-of-calls-to-fail* 0))))

(define with-independent-scheduler
  (let ((with-independent-scheduler with-independent-scheduler))
    (lambda args
      (fluid-let ((*number-of-calls-to-fail* #f))
	(apply with-independent-scheduler args)))))


(define *contradiction-wallp* #f)

(define (process-nogood! nogood)
  (set! *number-of-calls-to-fail*
        (+ *number-of-calls-to-fail* 1))
  (process-one-contradiction nogood))
