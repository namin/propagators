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

(define *explain-debug* #f)

(define (explain cell #!optional depth)
  (assert (cell? cell) "Can only explain a cell.")
  (if (default-object? depth) (set! depth 100))
  (let ((n (+ (diagram-depth cell) depth))
	(mark (make-eq-hash-table)))
    (define (reason diagram)
      (cond  ((symbol? diagram) (list diagram))
	     ((diagram? diagram)
	      (let ((eadiagram (explanation-ancestor diagram depth)))
		(cons (name-of eadiagram)
		      (map name-of
			   (diagram-inputs eadiagram)))))
	     (else
	      (error "Unknown informant type -- EXPLAIN"
		     (name cell)))))
    (define (explain-cell cell c)
      (if (and (< (diagram-depth cell) n)
	       (or (not (= (length (name-of cell)) 1))
		   (not (number? (car (name-of cell))))))
	  (if (nothing? c)
	      `(,(name-of cell) has ,c)
	      `(,(name-of cell)
		,@(if *explain-debug* (list (hash cell)) '())
		has-value ,(v&s-value c)
		,@(let ((infs (v&s-informants c)))
		    (if (null? infs)
			'()
			(cons 'by (map reason infs))))
		,@(if (null? (v&s-support c))
		      '()
		      (cons 'with-premises (v&s-support c)))))
	  #f))
    (define (explain cell)
      (let ((seen (hash-table/get mark cell #f)))
	(if (not seen)
	    (let ((c (tms-query (->tms (content cell)))))
	      (let ((infs (if (nothing? c) '() (v&s-informants c))))
		(hash-table/put! mark cell #t)
		(let ((e (explain-cell cell c))
		      (r (append-map
			  (lambda (inf)
			    (if (symbol? inf)
				'()
				(append-map explain
				  (diagram-inputs
				   (explanation-ancestor inf depth)))))
			  infs)))
		  (if e (cons e r) r))))
	    '())))
    (explain cell)))

(define (diagram-depth thing)
  (cond ((diagram-creator thing)
	 => (lambda (d)
	      (+ 1 (diagram-depth d))))
	(else 0)))
    
(define (explanation-ancestor thing n)
  (let lp ((thing thing) (m (- (diagram-depth thing) n)))
    (if (<= m 0)
	thing
	(lp (diagram-creator thing) (- m 1)))))

#|
;;; Here is an example:

(what-is M87:distance)
#| ((+- 19.5 2.678) depends-on VanDenBergh1985) |#

(cpp (explain M87:distance 1))
#|
(((m87:distance) has-value #[interval 16.83 22.18] by ((c:mu<->d) (m87:distance-modulus)) with-premises vandenbergh1985)
 ((m87:distance-modulus) has-value #[interval 31.13 31.73] by (user) with-premises vandenbergh1985))
|#

(cpp (explain M87:distance 2))
#|
(((m87:distance) has-value #[interval 16.83 22.18] by ((p:mu->d) (m87:distance-modulus)) with-premises vandenbergh1985)
 ((m87:distance-modulus) has-value #[interval 31.13 31.73] by (user) with-premises vandenbergh1985))
|#

(cpp (explain M87:distance 3))
#|
(((m87:distance) has-value #[interval 16.83 22.18] by ((p:/) (1000000.) (cell160)) with-premises vandenbergh1985)
 ((cell160) has-value #[interval 16830000. 22180000.] by ((exp:p) (cell157)) with-premises vandenbergh1985)
 ((cell157) has-value #[interval 16.64 16.91] by ((p:*) (l10) (cell156)) with-premises vandenbergh1985)
 ((cell156) has-value #[interval 7.226 7.346] by ((+:p) (cell151) (1)) with-premises vandenbergh1985)
 ((cell151) has-value #[interval 6.226 6.346] by ((p:/) (5) (m87:distance-modulus)) with-premises vandenbergh1985)
 ((m87:distance-modulus) has-value #[interval 31.13 31.73] by (user) with-premises vandenbergh1985)
 ((l10) has-value 2.303))
|#

(cpp (explain M87:distance 4))
#|
(((m87:distance) has-value #[interval 16.83 22.18] by ((/:p) (cell160) (1000000.)) with-premises vandenbergh1985)
 ((cell160) has-value #[interval 16830000. 22180000.] by ((exp:p) (cell157)) with-premises vandenbergh1985)
 ((cell157) has-value #[interval 16.64 16.91] by ((*:p) (cell156) (l10)) with-premises vandenbergh1985)
 ((cell156) has-value #[interval 7.226 7.346] by ((+:p) (cell151) (1)) with-premises vandenbergh1985)
 ((cell151) has-value #[interval 6.226 6.346] by ((/:p) (m87:distance-modulus) (5)) with-premises vandenbergh1985)
 ((m87:distance-modulus) has-value #[interval 31.13 31.73] by (user) with-premises vandenbergh1985)
 ((l10) has-value 2.303))
|#
|#
