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

;;;; Basic scheduling system

;;; This scheduler maintains a list of jobs that need to be run.  Each
;;; job is a thunk.  Jobs are run serially and are not preempted.
;;; When a job exits (normally) it is forgotten and the next job is
;;; run.  The jobs are permitted to schedule additional jobs,
;;; including rescheduling themselves.  Jobs are presumed idempotent,
;;; and specifically it is assumed acceptable not to count how many
;;; times a given job (by eq?-ness) was scheduled, but merely that it
;;; was scheduled.  When the scheduler runs out of jobs, it returns
;;; the symbol 'DONE to its caller.

;;; The scheduler supplies an escape mechanism: running the procedure
;;; ABORT-PROCESS, with a value, will terminate the entire job run,
;;; and return the supplied value to the scheduler's caller.
;;; Subsequent calls to the scheduler without first scheduling more
;;; jobs will also return that same value.  If ABORT-PROCESS is called
;;; outside the dynamic extent of a run, it deschedules any jobs that
;;; might be scheduled and saves the value for future reference as
;;; above.

;;; This scheduler is meant as a low-level support for the propagator
;;; network in this prototype.  In that use case, the jobs would be
;;; propagators that the network knows need to be run.  Any cells in
;;; the network are invisible to the scheduler, but presumably help
;;; the network schedule more propagators to run (namely those that
;;; may be interested in the cell's goings on).

;;; The main public interface is
;;;   (initialize-scheduler)      clear all scheduler state
;;;   (alert-propagators jobs)    schedule a list (or set) of jobs
;;;   (alert-all-propagators!)    reschedule all jobs ever scheduled
;;;   (run)                       run scheduled jobs until done
;;;   (abort-process x)           terminate the run returning x

;;; The scheduler also provides
;;; (with-independent-scheduler thunk)
;;;   Run thunk in a fresh scheduler, then restore current scheduler.
;;; (make-scheduler) 
;;;   Mutation point that can be configured to expriment with
;;;   different scheduling strategies.
;;; (execute-propagator propagator)
;;;   Execute a propagator immediately rather than scheduling it for
;;;   later.  Use judiciously.
;;; (all-propagators)
;;;   Returns a list of all known propagators.  Mainly for debugging
;;;   a propagator network.

(define *scheduler*)
(define *abort-process*)
(define *last-value-of-run*)
(define *propagators-ever-alerted*)

;; This is a mutation point, if one wants to play with different kinds
;; of schedulers.  The default is round-robin, below.
(define (make-scheduler) (make-round-robin-scheduler))

(define (initialize-scheduler)
  (set! *scheduler* (make-scheduler))
  (set! *abort-process* #f)
  (set! *last-value-of-run* 'done)
  (set! *propagators-ever-alerted* (make-eq-oset))
  'ok)

(define (with-independent-scheduler thunk)
  (fluid-let ((*scheduler* #f)
	      (*abort-process* #f)
	      (*last-value-of-run* #f)
	      (*propagators-ever-alerted* #f))
    (initialize-scheduler)
    (thunk)))

(define (execute-propagator propagator)
  (propagator))

(define (alert-propagators propagators)
  (for-each
   (lambda (propagator)
     (if (not (procedure? propagator))
         (error "Alerting a non-procedure" propagator))
     (oset-insert *propagators-ever-alerted* propagator)
     ((*scheduler* 'alert-one) propagator))
   (listify propagators))
  #f)
(define alert-propagator alert-propagators)

(define (all-propagators)
  (oset-members *propagators-ever-alerted*))

(define (alert-all-propagators!)
  (for-each (*scheduler* 'alert-one) (all-propagators)))

(define (with-process-abortion thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((*abort-process* k))
       (thunk)))))

(define termination-trace #f)

(define (abort-process value)
  (if termination-trace
      (ppc `(calling abort-process with ,value and ,*abort-process*)))
  (if *abort-process*
      ;; if the propagator is running
      (begin (*scheduler* 'clear!)
             (*abort-process* value))
      ;; if the user is setting up state
      (begin (*scheduler* 'clear!)
             (set! *last-value-of-run* value))))

(define (run)
  (define (do-run)
    (*scheduler* 'run))
  (if (not (*scheduler* 'done?))
      (set! *last-value-of-run* (with-process-abortion do-run)))
  *last-value-of-run*)

(define (make-oset-scheduler policy)
  (let ((propagators-left (make-eq-oset)))
    (define (run-alerted)
      (if (any-alerted?)
	  (begin
	    (policy propagators-left)
	    (run-alerted))
	  'done))

    (define (alert-one propagator)
      (oset-insert propagators-left propagator))

    (define (clear!)
      (oset-clear! propagators-left))

    (define (any-alerted?)
      (< 0 (oset-count propagators-left)))

    (define (me message)
      (cond ((eq? message 'run) (run-alerted))
	    ((eq? message 'alert-one) alert-one)
	    ((eq? message 'clear!) (clear!))
	    ((eq? message 'done?) (not (any-alerted?)))))
    me))

(define (round-robin-policy propagators-left)
  (let ((temp (oset-members propagators-left)))
    (oset-clear! propagators-left)
    (for-each (lambda (propagator)
		(execute-propagator propagator))
	      temp)))

(define (stack-policy propagators-left)
  (execute-propagator (oset-pop! propagators-left)))

(define (make-round-robin-scheduler)
  (make-oset-scheduler round-robin-policy))

(define (make-stack-scheduler)
  (make-oset-scheduler stack-policy))

(define (make-fast-slow-scheduler fast-policy slow-policy)
  (let ((propagators-left (make-eq-oset))
	(slow-propagators (make-eq-oset)))
    (define (run-alerted)
      (cond ((any-normal?)
	     (fast-policy propagators-left)
	     (run-alerted))
	    ((any-slow?)
	     (slow-policy slow-propagators)
	     (run-alerted))
	    (else 'done)))

    (define (alert-one propagator)
      (if (tagged-slow? propagator)
	  (oset-insert slow-propagators propagator)
	  (oset-insert propagators-left propagator)))

    (define (clear!)
      (oset-clear! propagators-left)
      (oset-clear! slow-propagators))

    (define (any-alerted?)
      (or (any-normal?) (any-slow?)))

    (define (any-normal?)
      (< 0 (oset-count propagators-left)))

    (define (any-slow?)
      (< 0 (oset-count slow-propagators)))

    (define (me message)
      (cond ((eq? message 'run) (run-alerted))
	    ((eq? message 'alert-one) alert-one)
	    ((eq? message 'clear!) (clear!))
	    ((eq? message 'done?) (not (any-alerted?)))))
    me))

(define (tagged-slow? thing)
  (eq-get thing 'slow))

(define (tag-slow! thing)
  (eq-put! thing 'slow #t)
  thing)

;;; These schedulers were much worse than round-robin on the slow
;;; examples when tagging amb-choose propagators as slow.
(define (make-two-stack-scheduler)
  (make-fast-slow-scheduler stack-policy stack-policy))
(define (make-robin-stack-scheduler)
  (make-fast-slow-scheduler round-robin-policy stack-policy))
(define (make-two-robin-scheduler)
  (make-fast-slow-scheduler round-robin-policy round-robin-policy))
