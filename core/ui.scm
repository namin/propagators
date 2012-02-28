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

;;; Some nice user interface code, to be improved.

(declare (usual-integrations make-cell cell?))

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)

;;; Make for nice transcripts.
(define (cpp x)
  (display "#|\n")
  (pp x)
  (display "|#\n"))

;;; This is part of paranoid programming.
(define (assert p #!optional error-comment irritant)
  (if (not p)
      (begin
	(if (not (default-object? irritant))
	    (pp irritant))
	(error
	 (if (default-object? error-comment)
	     "Failed assertion"
	     error-comment)))))

;;; This is required because (run) returns old value if there is
;;; nothing to do.  This is a problem if a contradiction is resolved
;;; by a kick-out! with no propagation.

(define (tell! cell information . premises)
  (assert (cell? cell) "Can only tell something to a cell.")
  (set! *last-value-of-run* 'done)
  (add-content cell
	       (make-tms
		(contingent information premises)))
  (run))

(define (retract! premise)
  (set! *last-value-of-run* 'done)
  (kick-out! premise)
  (run))

(define (assert! premise)
  (set! *last-value-of-run* 'done)
  (bring-in! premise)
  (run))

(define (inquire cell)
  (assert (cell? cell) "Can only inquire of a cell.")
  (let ((v (run)))
    (if (not (eq? v 'done)) (write-line v)))
  (list (name-of cell)
	'has 
	(tms-query (->tms (content cell)))))

(define (name-of thing)
  (let ((n (eq-get thing 'given-name)))
    (if n
	(let ((n (if (list? n) (map name-of n) n))
	      (p (eq-get thing 'parent)))
	  (if p
	      (cons n (name-of p))
	      (list n)))
	(list (name thing)))))


;;; For debugging purposes

(define (probe! cell thunk)
  ;; thunk = (lambda () (lambda (cell) ...))
  (define (the-probe)
    ((thunk) cell))
  ((cell 'probe!) thunk))

(define (unprobe! cell)
  (cell 'unprobe))

#|
;;; Superseded by explain.scm

(define (explain cell)
  (assert (cell? cell) "Can only explain a cell.")
  (let ((mark (make-eq-hash-table)))
    (define (explain-cell cell c)
      `(,(name-of cell)
	has-value ,(v&s-value c)
	,@(let ((infs (v&s-informants c)))
	    (if (null? infs)
		'()
		(cons 'by
		      (map (lambda (inf)
			     (if (symbol? inf)
				 (list inf)
				 (cons (name-of inf)
				       (map name-of
					    (eq-get inf 'inputs)))))
			   infs))))
	,@(if (null? (v&s-support c))
	      '()
	      (cons 'with-premises (v&s-support c)))))
    (define (explain cell)
      (let ((seen (hash-table/get mark cell #f)))
	(if (not seen)
	    (let* ((c (tms-query (->tms (content cell))))
		   (infs (v&s-informants c)))
	      (hash-table/put! mark cell #t)
	      (cons (explain-cell cell c)
		    (append-map
		     (lambda (inf)
		       (if (symbol? inf)
			   '()
			   (append-map explain
				       (eq-get inf 'inputs))))
		     infs)))
	    '())))
    (explain cell)))
|#

#|
;;;; A Small Financial Example 

;;; First, we need a small database mechanism
;;;  Parent and child here do not refer to biological
;;;  things, but rather the relationships of parts
;;;  of a database.

(define (add-branch! parent child name)
  (eq-put! parent name child)
  (eq-put! child 'parent parent)
  (eq-put! child 'given-name name)
  'done)

;;; e.g. (thing-of Gaggle-salary gross-income Ben)

(define (thing-of name-path)
  (let lp ((path name-path))
    (cond ((= (length path) 1) (car path))
	  (else
	   (eq-get (lp (cdr path))
		   (car path))))))



;;; A financial entity has three cells

(define (make-financial-entity entity)
  (eq-put! entity 'kind-of-entity 'financial)

  (let-cells (gross-income expenses net-income)

    (add-branch! entity gross-income 'gross-income)
    (add-branch! entity net-income 'net-income)
    (add-branch! entity expenses 'expenses)

    (c:+ expenses net-income gross-income)
    'done
    ))

(define (financial-entity? thing)
  (eq? (eq-get thing 'kind-of-entity) 'financial))

(define (gross-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'gross-income))

(define (net-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'net-income))

(define (expenses entity)
  (assert (financial-entity? entity))
  (eq-get entity 'expenses))

(define (breakdown sum-node . part-names)
  (for-each (lambda (part-name)
	      (let-cell part
			(add-branch! sum-node part part-name)))
	    part-names)
  (cond ((= (length part-names) 2)
	 (c:+ (eq-get sum-node (car part-names))
	      (eq-get sum-node (cadr part-names))
	      sum-node)
	 'done)
	(else
	 (error "I don't know how to sum multiple parts"))))
	      
(define (combine-financial-entities compound . parts)
  (assert (every financial-entity? parts))
  (cond ((= (length parts) 2)
	 (let ((p1 (car parts)) (p2 (cadr parts)))
	   (c:+ (gross-income p1) (gross-income p2) (gross-income compound))
	   (c:+ (net-income p1) (net-income p2) (net-income compound))
	   (c:+ (expenses p1) (expenses p2) (expenses compound))
	   'done))
	(else
	 (error "I don't know how to combine multiple parts"))))

#|
(initialize-scheduler)

(make-financial-entity 'Alyssa)
(make-financial-entity 'Ben)

;;; Ben and Alyssa are married
(make-financial-entity 'Ben-Alyssa)
(combine-financial-entities 'Ben-Alyssa 'Ben 'Alyssa)

;;; Ben and Alyssa file income tax jointly
(tell! (gross-income 'Ben-Alyssa) 427000 'IRS)

;;; Ben works at Gaggle as a software engineer.
(breakdown (gross-income 'Ben) 'Gaggle-salary 'investments)

;;; He gets paid alot to make good apps.
(tell! (thing-of '(Gaggle-salary gross-income Ben)) 200000 'Gaggle)

;;; Alyssa works as a PhD biochemist in big pharma.
(breakdown (gross-income 'Alyssa) 'GeneScam-salary 'investments)

;;; Biochemists are paid poorly.
(tell! (thing-of '(GeneScam-salary gross-income Alyssa)) 70000 'GeneScam)

(tell! (thing-of '(investments gross-income Alyssa))
       (make-interval 30000 40000) 'Alyssa)

(cpp (inquire (thing-of '(investments gross-income Ben))))
#|
((investments gross-income ben)
 has
 #(value=#[interval 117000 127000], premises=(gaggle genescam alyssa irs), informants=((-:p gross-income part))))
|#

;;; Ben is a tightwad
(tell! (thing-of '(expenses Ben)) (make-interval 10000 20000) 'Ben)

(cpp (inquire (thing-of '(net-income Ben))))
#|
((net-income ben)
 has
 #(value=#[interval 297000 317000], premises=(ben genescam alyssa irs), informants=((-:p gross-income expenses))))
|#

;;; But Alyssa is not cheap.  She likes luxury.
(tell! (thing-of '(expenses Alyssa)) (make-interval 200000 215000) 'Alyssa)

(cpp (inquire (thing-of '(net-income Alyssa))))
#|
((net-income alyssa)
 has
 #(value=#[interval -115000 -90000], premises=(alyssa genescam), informants=((-:p gross-income expenses))))
|#

;;; But they are doing OK anyway!
(cpp (inquire (thing-of '(net-income Ben-Alyssa))))
#|
((net-income ben-alyssa)
 has
 #(value=#[interval 192000 217000], premises=(ben alyssa irs), informants=((-:p gross-income expenses))))
|#

;;; Notice that this conclusion does not depend on the details, such
;;; as Gaggle or GeneScam!

(cpp (explain (thing-of '(net-income Ben-Alyssa))))
#|
(((net-income ben-alyssa) has-value #[interval 192000 217000] by ((-:p) (gross-income ben-alyssa) (expenses ben-alyssa)) with-premises ben alyssa irs)
 ((gross-income ben-alyssa) has-value 427000 by (user) with-premises irs)
 ((expenses ben-alyssa) has-value #[interval 210000 235000] by ((+:p) (expenses ben) (expenses alyssa)) with-premises alyssa ben)
 ((expenses ben) has-value #[interval 10000 20000] by (user) with-premises ben)
 ((expenses alyssa) has-value #[interval 200000 215000] by (user) with-premises alyssa))
|#
|#
|#