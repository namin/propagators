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

#|
;;; This causes real trouble with pretty-printing
(define-structure
  (tms (type vector) (named 'tms)
       (constructor %make-tms) (print-procedure #f)
       (safe-accessors #t))
  values)
|#

(define (%make-tms values)
  (vector 'tms values))

(define (tms? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tms)))

(define (tms-values tms)
  (if (not (tms? tms)) (error "Bad tms -- TMS-VALUES" tms))
  (vector-ref tms 1))

(define (set-tms-values! tms new-values)
  (if (not (tms? tms)) (error "Bad tms -- SET-TMS-VALUES!" tms))
  (vector-set! tms 1 new-values))


(define (make-tms arg)
  (%make-tms (listify arg)))

;; Will be replaced by tms-merge in contradictions.scm
(define (tms-merge tms1 tms2)
  (let ((candidate (tms-assimilate tms1 tms2)))
    (effectful-bind (strongest-consequence candidate)
      (lambda (consequence)
	(tms-assimilate candidate consequence)))))

(define (tms-assimilate tms stuff)
  (cond ((nothing? stuff) tms)
        ((v&s? stuff) (tms-assimilate-one tms stuff))
        ((tms? stuff)
         (fold-left tms-assimilate-one
                    tms
                    (tms-values stuff)))
        (else (error "This should never happen" stuff))))

(define (subsumes? v&s1 v&s2)
  (and (lset<= eq? (v&s-support v&s1) (v&s-support v&s2))
       (implies? (v&s-value v&s1) (v&s-value v&s2))))

(define (tms-assimilate-one tms v&s)
  (if (any (lambda (old-v&s) (subsumes? old-v&s v&s))
           (tms-values tms))
      tms
      (let ((subsumed
             (filter (lambda (old-v&s) (subsumes? v&s old-v&s))
                     (tms-values tms))))
        (make-tms
         (lset-adjoin eq?
           (lset-difference eq? (tms-values tms) subsumed)
           v&s)))))

(define (strongest-consequence tms)
  (let ((cached (cached-consequence tms)))
    (or cached
	(cache-consequence! tms (compute-strongest-consequence tms)))))

(define *consequence-cache* (make-eq-hash-table))

(define (cached-consequence tms)
  (let ((answer (hash-table/get *consequence-cache* tms #f)))
    (and answer
	 (= (car answer) *worldview-number*)
	 (cdr answer))))

(define (cache-consequence! tms consequence)
  (hash-table/put! *consequence-cache* tms
   ;; Caching the data, not the effect (if any)
   (cons *worldview-number* (effectful-info (->effectful consequence))))
  consequence)

(define (compute-strongest-consequence tms)
  (let ((relevant-v&ss
         (filter v&s-believed? (tms-values tms))))
    (merge* relevant-v&ss)))

(define (v&s-believed? v&s)
  (all-premises-in? (v&s-support v&s)))
(define contingency-object-believed? v&s-believed?)

(define (all-premises-in? premise-list)
   (every premise-in? premise-list))

(define initialize-scheduler
  (let ((initialize-scheduler initialize-scheduler))
    (lambda ()
      (initialize-scheduler)
      (set! *consequence-cache* (make-eq-hash-table)))))

(define with-independent-scheduler
  (let ((with-independent-scheduler with-independent-scheduler))
    (lambda args
      (fluid-let ((*consequence-cache* #f))
	(apply with-independent-scheduler args)))))

;; Will be replaced by tms-query in contradictions.scm
(define (tms-query tms)
  (let ((answer (strongest-consequence tms)))
    (let ((better-tms (tms-assimilate tms answer)))
      (if (not (eq? tms better-tms))
          (set-tms-values! tms (tms-values better-tms)))
      answer)))

(define (kick-out! premise)
  (if (premise-in? premise)
      (begin
	(set! *worldview-number* (+ *worldview-number* 1))
	(alert-all-propagators!)))
  (mark-premise-out! premise))

(define (bring-in! premise)
  (if (not (premise-in? premise))
      (begin
	(set! *worldview-number* (+ *worldview-number* 1))
	(alert-all-propagators!)))
  (mark-premise-in! premise))

(defhandler generic-unpack
  (lambda (tms function)
    (let ((relevant-information (tms-query tms)))
      (make-tms (list (generic-bind relevant-information function)))))
  tms? any?)

(defhandler generic-flatten
  (lambda (tms)
    (tms->
     (make-tms
      (append-map tms-values
		  (map ->tms
		       (map generic-flatten (tms-values tms)))))))
  tms?)

(defhandler generic-flatten
  (lambda (v&s)
    (generic-flatten
     (make-tms
      (generic-flatten
       (supported (tms-query (v&s-value v&s))
		  (v&s-support v&s)
		  (v&s-informants v&s))))))
  (lambda (thing) (and (v&s? thing) (tms? (v&s-value thing)))))

(declare-coercion-target tms
  (lambda (thing)
    (make-tms (list (->contingent thing)))))

(declare-coercion v&s? ->tms)
(declare-coercion contingent-able? ->tms)
(defhandler ->tms (lambda (nothing) (make-tms '())) nothing?)

(define (tms-equivalent? tms1 tms2)
  (lset= v&s-equivalent? (tms-values tms1) (tms-values tms2)))
(defhandler-coercing equivalent? tms-equivalent? ->tms)

(define (the-tms-handler thing1 thing2)
  (tms-merge thing1 thing2))

(defhandler-coercing merge the-tms-handler ->tms)

(define (tms-> tms)
  (let ((values (filter v&s? (map v&s-> (map ->contingent (tms-values tms))))))
    (cond ((null? values)
	   nothing)
	  ((and (= 1 (length values))
		(v&s? (car values))
		(null? (v&s-support (car values))))
	   (v&s-value (car values)))
	  (else
	   (make-tms values)))))

(define (tms-binary-map tms1 tms2)
  (lambda (f)
    (tms-> (make-tms (list (f (tms-query tms1) (tms-query tms2)))))))

(defhandler-coercing binary-map tms-binary-map ->tms)
