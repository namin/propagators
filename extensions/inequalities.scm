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

;;; Strategy:
;;; 
;;; Use equations to solve for variables
;;; 
;;; Substitute into the inequalities thoroughly
;;; 
;;; When the time comes, check the inequalities for consistency by
;;;   looking for ones where a variable appears alone
;;;   solving for that variable
;;;   isolating the strongest inequality in that variable in each direction
;;;     (the rest can be discarded as redundant)
;;;   checking whether the resulting interval is empty

(declare (usual-integrations))

(define (symbolic-variable? thing)
  (and (symbol? thing)
       (not (memq thing '(+ - / * D expt sqrt exp sin cos < > <= >=)))))

(define (find-variables expr)
  (define (tree-fringe tree)
    (let walk ((tree tree)
	       (answer '()))
      (cond ((pair? tree)
	     (walk (car tree) (walk (cdr tree) answer)))
	    ((null? tree) answer)
	    (else
	     (cons tree answer)))))
  (delete-duplicates (filter symbolic-variable? (tree-fringe expr))))

(define-structure (inequality (constructor %%make-inequality) safe-accessors)
  direction
  expr1
  expr2
  variables)

(define (inequality->list ineq)
  `(,(inequality-direction ineq)
    ,(inequality-expr1 ineq)
    ,(inequality-expr2 ineq)))

(define (list->inequality lst)
  (if (inequality? lst)
      lst
      (if (and (pair? lst)
	       (= 3 (length lst))
	       (memq (car lst) '(< > <= >=)))
	  (%make-inequality (car lst) (cadr lst) (caddr lst))
	  (error "Given object does not look like an inequality" lst))))

(define (%make-inequality dir expr1 expr2)
  (if (not (memq dir '(< <= > >=)))
      (error "Unsupported direction" dir))
  (let ((expr1 (simplify expr1))
	(expr2 (simplify expr2)))
    (%%make-inequality dir expr1 expr2 (find-variables (cons expr1 expr2)))))

(define (make-inequality dir expr)
  (%make-inequality dir expr 0))

(define (make-solved-inequality dir var answer)
  (if (or (not (symbolic-variable? var)) (not (number? answer)))
      (error "Incomplete solution" dir var answer))
  (%make-inequality dir var answer))

(define (inequality-expression ineq)
  (if (and (number? (inequality-expr2 ineq))
	   (= 0 (inequality-expr2 ineq)))
      (inequality-expr1 ineq)
      (simplify
       (symb:- (inequality-expr1 ineq)
	       (inequality-expr2 ineq)))))
(define (the-ineq-variable ineq)
  (if (= 1 (length (inequality-variables ineq)))
      (car (inequality-variables ineq))
      (error "No unique variable in" ineq)))

(define (normalized-ineq? ineq)
  (number? (inequality-expr2 ineq)))

(define (normalize-ineq ineq)
  (if (normalized-ineq? ineq)
      ineq
      (make-inequality
       (inequality-direction ineq)
       (inequality-expression ineq))))

(define (solved-ineq? ineq)
  (and (symbolic-variable? (inequality-expr1 ineq))
       (number? (inequality-expr2 ineq))))

(define (determined-ineq? ineq)
  (= 0 (length (inequality-variables ineq))))

(define (evaluate-ineq ineq)
  (if (not (determined-ineq? ineq))
      (error "Cannot evaluate undetermined inequality" ineq))
  (eval
   (list (inequality-direction ineq)
	 (inequality-expr1 ineq)
	 (inequality-expr2 ineq))
   (nearest-repl/environment)))

(define (tautological-ineq? ineq)
  (and (determined-ineq? ineq)
       (evaluate-ineq ineq)))

(define (contradictory-ineq? ineq)
  (and (determined-ineq? ineq)
       (not (evaluate-ineq ineq))))

(define (make-tautological-ineq)
  (make-inequality '<= 0))

(define (make-contradictory-ineq)
  (make-inequality '< 0))

;;; This is the main interface to the inequality solver.  Given a list
;;; of inequalities, it either returns a simplified list of
;;; inequalities, of #f if the inequalities are inconsistent.
(define (solve-inequalities inequalities)
  (let ((answer (simplify-inequalities (map list->inequality inequalities))))
    (and answer (map inequality->list answer))))

(define (simplify-inequalities inequalities)
  (let loop ((inequalities (map simplify-ineq inequalities))
	     (solved '())
	     (unsolved '()))
    (if (null? inequalities)
	(consistent-subset (append unsolved solved))	
	(try-inequality
	 (car inequalities)
	 (lambda (deduction)
	   (cond ((tautological-ineq? deduction)
		  (loop (cdr inequalities) solved unsolved))
		 ((contradictory-ineq? deduction)
		  #f)
		 ((solved-ineq? deduction)
		  (loop (cdr inequalities)
			(cons deduction solved)
			unsolved))
		 (else
		  (loop (cdr inequalities)
			solved 
			(cons deduction unsolved)))))
	 (lambda ()
	   (loop (cdr inequalities)
		 solved 
		 (cons (car inequalities) unsolved)))))))

(define (consistent-subset inequalities)
  (let ((inequalities (map normalize-ineq inequalities)))
    (let loop ((expressions
		(delete-duplicates
		 (map inequality-expr1 inequalities)))
	       (answer '()))
      (if (null? expressions)
	  answer
	  (let ((one-expr-consistent
		 (consistent-subset-one-expr
		  (filter (lambda (ineq)
			    (equal? (car expressions)
				    (inequality-expr1 ineq)))
			  inequalities))))
	    (and one-expr-consistent
		 (loop (cdr expressions)
		       (append one-expr-consistent answer))))))))

(define (minimum lst <)
  (if (null? lst)
      #f
      (let loop ((min (car lst))
		 (rest (cdr lst)))
	(if (null? rest)
	    min
	    (if (< min (car rest))
		(loop min (cdr rest))
		(loop (car rest) (cdr rest)))))))

(define (upper-bound-ineq? ineq)
  (memq (inequality-direction ineq) '(< <=)))

(define (lower-bound-ineq? ineq)
  (memq (inequality-direction ineq) '(> >=)))

(define (stricter-upper-bound? ineq1 ineq2)
  (and (equal? (inequality-expr1 ineq1)
	       (inequality-expr1 ineq2))
       (or (< (inequality-expr2 ineq1)
	      (inequality-expr2 ineq2))
	   (and (= (inequality-expr2 ineq1)
		   (inequality-expr2 ineq2))
		(or (eq? '< (inequality-direction ineq1))
		    (and (eq? '<= (inequality-direction ineq1))
			 (eq? '<= (inequality-direction ineq2))))))))

(define (stricter-lower-bound? ineq1 ineq2)
  (and (equal? (inequality-expr1 ineq1)
	       (inequality-expr1 ineq2))
       (or (> (inequality-expr2 ineq1)
	      (inequality-expr2 ineq2))
	   (and (= (inequality-expr2 ineq1)
		   (inequality-expr2 ineq2))
		(or (eq? '> (inequality-direction ineq1))
		    (and (eq? '>= (inequality-direction ineq1))
			 (eq? '>= (inequality-direction ineq2))))))))

(define (consistent-subset-one-expr inequalities)
  (let ((best-upper-bound
	 (minimum (filter upper-bound-ineq? inequalities)
		  stricter-upper-bound?))
	(best-lower-bound
	 (minimum (filter lower-bound-ineq? inequalities)
		  stricter-lower-bound?)))
    (cond ((and best-upper-bound best-lower-bound)
	   (let ((consistent?
		  (evaluate-ineq
		   (transitive-ineq best-lower-bound best-upper-bound))))
	     (if consistent?
		 (list best-lower-bound best-upper-bound)
		 #f)))
	  (best-upper-bound (list best-upper-bound))
	  (best-lower-bound (list best-lower-bound))
	  (else '()))))

(define (transitive-ineq lower-bound upper-bound)
  (cond ((not (lower-bound-ineq? lower-bound))
	 (error "Not a lower bound" lower-bound))
	((not (upper-bound-ineq? upper-bound))
	 (error "Not an upper bound" upper-bound))
	((not (equal? (inequality-expr1 lower-bound)
		      (inequality-expr1 upper-bound)))
	 (error "Inappropriate attempt at transitivity" lower-bound upper-bound))
	(else
	 (make-inequality (joint-operation (inequality-direction lower-bound)
					   (inequality-direction upper-bound))
			  (simplify
			   (symb:- (inequality-expr2 lower-bound)
				   (inequality-expr2 upper-bound)))))))

(define (joint-operation lower-dir upper-dir)
  (if (and (eq? '>= lower-dir) (eq? '<= upper-dir))
      '<=
      '<))

(define (try-inequality ineq succeed fail)
  (if (or (solved-ineq? ineq)
	  (tautological-ineq? ineq)
	  (contradictory-ineq? ineq))
      (succeed ineq)
      (if (> (length (inequality-variables ineq)) 1)
	  (fail)
	  (try-ineq-expression
	   (the-ineq-variable ineq)
	   (inequality-direction ineq)
	   (inequality-expression ineq)
	   succeed fail))))

(define (try-ineq-expression var dir expr succeed fail)
  (cond ((equal? var expr)
	 (succeed (make-solved-inequality dir var 0)))
	((expt? expr)
	 (numerify
	  (cadr (operands expr))
	  (lambda (num)
	    (try-power var dir expr succeed fail))
	  fail))
	((product? expr)
	 (try-product var dir expr succeed fail))
	((sum? expr)
	 (try-sum var dir expr succeed fail))
	(else
	 (fail))))

(define (expr-exponent expr)
  (if (expt? expr)
      (cadr (operands expr))
      1))

(define (numerify possibly-symbolic-number succeed fail)
  (let ((answer (simplify possibly-symbolic-number)))
    (if (number? answer)
	(succeed answer)
	(fail))))

(define (try-power var dir expr succeed fail)
  (if (even? (cadr (operands expr)))
      (case dir
	((<)  (succeed (make-contradictory-ineq)))
	;; TODO (= expr 0)
	((<=) (succeed (make-contradictory-ineq)))
	((>=) (succeed (make-tautological-ineq)))
	;; TODO (<> expr 0)
	((>)  (succeed (make-tautological-ineq)))
	(else
	 (error "Invalid direction" dir)))
      (try-ineq-expression var dir (car (operands expr)) succeed fail)))

(define (reverse-sense dir)
  (case dir
    ((<)  '>)
    ((<=) '>=)
    ((>=) '<=)
    ((>)  '<)
    (else
     (error "Invalid direction" dir))))

(define (negate-sense dir)
  (case dir
    ((<)  '>=)
    ((<=) '>)
    ((>=) '<)
    ((>)  '<=)
    (else
     (error "Invalid direction" dir))))

(define (try-product var dir expr succeed fail)
  (let loop ((factors (operands expr))
	     (powers-of-var '())
	     (constants '()))
    (cond ((null? factors)
	   (numerify
	    (symb:add:n (map expr-exponent powers-of-var))
	    (lambda (power)
	      (numerify 
	       (symb:mul:n constants)
	       (lambda (factor)
		 (cond ((= factor 0)
			(succeed (make-inequality dir 0)))
		       ((> factor 0)
			(try-power var dir
				   `(expt ,var ,power) succeed fail))
		       ((< factor 0)
			(try-power var (reverse-sense dir)
				   `(expt ,var ,power) succeed fail))))
	       fail))
	    fail))
	  ((occurs? var (car factors))
	   (if (or (equal? var (car factors))
		   (and (expt? (car factors))
			(equal var (car (operands factors)))))
	       (loop (cdr factors)
		     (cons (car factors) powers-of-var)
		     constants)
	       ;; Otherwise the presence of var is too complicated
	       (fail)))
	  (else
	   (loop (cdr factors)
		 powers-of-var
		 (cons (car factors) constants))))))

(define (try-sum var dir expr succeed fail)
  (let loop ((addends (operands expr))
	     (coefficients-of-var '())
	     (constants '()))
    (cond ((null? addends)
	   (numerify
	    (symb:add:n coefficients-of-var)
	    (lambda (coeff)
	      (numerify 
	       (symb:add:n constants)
	       (lambda (constant)
		 (cond ((= coeff 0)
			(succeed (make-inequality dir constant)))
		       ((> coeff 0)
			(succeed
			 (make-solved-inequality
			  dir var (/ (- constant) coeff))))
		       ((< coeff 0)
			(succeed
			 (make-solved-inequality
			  (reverse-sense dir) var (/ (- constant) coeff))))))
	       fail))
	    fail))
	  ((occurs? var (car addends))
	   (cond ((equal? var (car addends))
		  (loop (cdr addends)
			(cons 1 coefficients-of-var)
			constants))
		 ((product? (car addends))
		  (let ((factors (operands (car addends))))
		    (if (member var factors)
			(loop (cdr addends)
			      (cons (symb:mul:n (delete var factors))
				    coefficients-of-var)
			      constants)
			;; Otherwise the presence of var is too complicated
			(fail))))
		 (else (fail))))
	  (else
	   (loop (cdr addends)
		 coefficients-of-var
		 (cons (car addends) constants))))))

(define (simplify-ineq ineq)
  (define (try-power dir expr num loop done)
    (numerify
     (cadr (operands expr))
     (lambda (power)
       (if (even? power)
	   ;; Even powers are not monotonic
	   (done)
	   (loop dir (car (operands expr)) (expt num (/ 1 power)))))
     done))
  (define (try-product dir expr num loop done)
    (let ((numbers (filter number? expr)))
      (if (null? numbers)
	  (done)
	  (let ((coeff (apply * numbers)))
	    ;; Assume coeff = 0 would have simplifed already
	    (loop (if (> coeff 0) dir (reverse-sense dir))
		  (simplify (symb:/ expr coeff))
		  (/ num coeff))))))
  (define (try-sum dir expr num loop done)
    (let ((numbers (filter number? expr)))
      (if (null? numbers)
	  (done)
	  (let ((addend (apply + numbers)))
	    (loop dir
		  (simplify (symb:- expr addend))
		  (- num addend))))))
  (let ((ineq (normalize-ineq ineq)))
    (if (determined-ineq? ineq)
	ineq
	(let loop ((dir (inequality-direction ineq))
		   (expr (inequality-expr1 ineq))
		   (num  (inequality-expr2 ineq)))
	  (define (done)
	    (%make-inequality dir expr num))
	  (cond ((expt? expr)
		 (try-power dir expr num loop done))
		((product? expr)
		 (try-product dir expr num loop done))
		((sum? expr)
		 (try-sum dir expr num loop done))
		(else (done)))))))
