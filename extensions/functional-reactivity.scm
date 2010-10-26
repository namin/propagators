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

(define-structure
  (frpremise
   (constructor make-frpremise (identity timestamp))
   (print-procedure
    (simple-unparser-method 'frp
     (lambda (frpremise)
       (list (frpremise-identity frpremise)
	     (frpremise-timestamp frpremise))))))
  identity
  timestamp
  (total-delay 0) ;; TODO Account for this
  )

(define (fr-same-input? frp1 frp2)
  (eq? (frpremise-identity frp1)
       (frpremise-identity frp2)))

(define (fr-invalidates? frp1 frp2)
  (and (fr-same-input? frp1 frp2)
       (> (frpremise-timestamp frp1)
	  (frpremise-timestamp frp2))))

(define-structure
  ;; For functional reactive supported
  (frs (constructor %make-frs (value support))
       (print-procedure
	(simple-unparser-method 'frs
	 (lambda (frs)
	   (list (frs-value frs) (frs-support frs))))))
  value
  support
  (stale #f))
(declare-type-tester frs? rtd:frs)

(define (make-frs value support)
  (%make-frs value (listify support)))

(define (make-stale-frs value support)
  (let ((answer (make-frs value support)))
    (set-frs-stale! answer #t)
    answer))

(define (stale-frs? thing)
  (and (frs? thing) (frs-stale thing)))
(declare-explicit-guard stale-frs? rtd:frs)

(define (fr-support-invalidates? frsupport1 frsupport2)
  (any (lambda (frp1)
	 (any (lambda (frp2)
		(fr-invalidates? frp1 frp2))
	      frsupport2))
       frsupport1))

(define (fr-merge-supports frsupport1 frsupport2)
  (lset-union fr-same-input? frsupport1 frsupport2))

(define (fr-more-recent? frsupport1 frsupport2)
  (>-list (map frpremise-timestamp frsupport1)
	  (map frpremise-timestamp frsupport2)))

(define (>-list lst1 lst2)
  (cond ((and (null? ls1) (null? ls2))
	 #f)
	((null? lst1)
	 #t)
	((null? lst2)
	 #f)
	((> (car lst1) (car lst2))
	 #t)
	((< (car lst1) (car lst2))
	 #t)
	(else
	 (>-list (cdr lst1) (cdr lst2)))))

(declare-coercion-target frs
  (lambda (thing) (make-frs thing '())))
(declare-coercion <number> ->frs)
(declare-coercion <symbol> ->frs)

(define (frs-binary-map frs1 frs2)
  (lambda (f)
    (let ((support1 (frs-support frs1))
	  (support2 (frs-support frs2)))
      (if (or (fr-support-invalidates? support1 support2)
	      (fr-support-invalidates? support2 support1))
	  nothing
	  (make-frs
	   (f (frs-value frs1) (frs-value frs2))
	   (fr-merge-supports support1 support2))))))

(defhandler-coercing binary-map frs-binary-map ->frs)

(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  frs? stale-frs?)

(defhandler binary-map
  (lambda (x y) (lambda (f) nothing))
  stale-frs? frs?)

(defhandler generic-unpack
  (lambda (frs function)
    (make-frs
     (generic-bind (frs-value frs) function)
     (frs-support frs)))
  frs? any?)

(defhandler generic-unpack
  (lambda (frs function) nothing)
  stale-frs? any?)

(defhandler generic-flatten
  (lambda (frs) nothing)
  (guard rtd:frs (lambda (thing) (nothing? (frs-value thing)))))

(defhandler generic-flatten
  (lambda (frs)
    (let ((support1 (frs-support frs))
	  (support2 (frs-support (frs-value frs))))
      (if (or (fr-support-invalidates? support1 support2)
	      (fr-support-invalidates? support2 support1))
	  nothing
	  (generic-flatten
	   (make-frs
	    (frs-value (frs-value frs))
	    (fr-merge-supports support1 support2))))))
  (guard rtd:frs (lambda (thing) (frs? (frs-value thing)))))

(define (merge-frs frs1 frs2)
  (let ((support1 (frs-support frs1))
	(support2 (frs-support frs2)))
    (cond ((and (fr-support-invalidates? support1 support2)
		(fr-support-invalidates? support2 support1))
	   (make-stale-frs
	    (frs-value frs1)
	    (fr-max-supports support1 support2)))
	  ((fr-support-invalidates? support1 support2)
	   frs1)
	  ((fr-support-invalidates? support2 support1)
	   frs2)
	  ;; These two are more policy decisions
	  ((fr-more-recent? support1 support2)
	   frs1)
	  ((fr-more-recent? support2 support1)
	   frs2)
	  (else
	   (let* ((frs1-value (frs-value frs1))
		  (frs2-value (frs-value frs2))
		  (value-merge (merge frs1-value frs2-value)))
	     (cond ((eq? value-merge frs1-value)
		    (if (implies? frs2-value value-merge)
			;; Confirmation of existing information
			#;
			(if (more-informative-support? frs2 frs1)
			    frs2
			    frs1)
			frs1
			;; New information is not interesting
			frs1))
		   ((eq? value-merge frs2-value)
		    ;; New information overrides old information
		    frs2)
		   (else
		    ;; Interesting merge, need both provenances
		    (make-frs value-merge
			      (fr-merge-supports support1 support2)))))))))

(defhandler merge merge-frs frs? frs?)
