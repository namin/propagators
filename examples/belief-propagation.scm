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

(declare (usual-integrations))

(define-structure message
  alist)
(declare-type-tester message? rtd:message)

;;; TODO Not really slotful: imposes invariant that the alist is
;;; an alist; thus order-independent.
(slotful-information-type message? make-message message-alist)

(define-method generic-match ((pattern <vector>) (object rtd:message))
  (generic-match
   pattern
   (vector 'message (message-alist object))))

(define (message-get message point)
  (cdr (assoc point (message-alist message))))

(define (message->alist message)
  (message-alist message))

(define (product numbers)
  (apply * numbers))

(define (sum numbers)
  (apply + numbers))

(define (pointwise-product support . messages)
  (make-message
   (map (lambda (point)
	  (cons point (product (map (lambda (message)
				      (message-get message point))
				    messages))))
	support)))

(propagatify pointwise-product 'no-generic)

(define (normalize-message message)
  (let ((constant (sum (map cdr (message-alist message)))))
    (make-message
     (map (lambda (pair)
	    (cons (car pair)
		  (/ (cdr pair) constant)))
	  (message-alist message)))))

(propagatify normalize-message)

(define (variable support marginal factor-terminals)
  (for-each
   (lambda (terminal)
     (let ((other-terminals (delq terminal factor-terminals)))
       (apply p:pointwise-product
	      `(,support ,@(map terminal-variable other-terminals)
			 ,(terminal-factor terminal)))))
   factor-terminals)
  (p:normalize-message
   (apply e:pointwise-product
	  support (map terminal-variable factor-terminals))
   marginal))

(define (pointwise-sum-product factor support . messages)
  (define (sum-product points-chosen messages-left)
    (if (null? messages-left)
	(apply factor (reverse points-chosen))
	(sum (map (lambda (point-value)
		    (* (cdr point-value)
		       (sum-product (cons (car point-value) points-chosen)
				    (cdr messages-left))))
		  (message->alist (car messages-left))))))
  (make-message
   (map (lambda (point)
	  (cons point (sum-product (list point) messages)))
	support)))

#|
 ;;; If John sends no influence to the John-Alarm factor, then the
 ;;; john-alarm factor should send no influence to the alarm variable.
 (pp
  (pointwise-sum-product
   (lambda (alarm john)
     (force-assoc
      (list alarm john)
      '(((#t #t) . .9)
	((#t #f) . .09999999999999998)
	((#f #t) . .05)
	((#f #f) . .95))))
   (list #t #f) ; Alarm's support
   (make-message '((#t . 1) (#f . 1))))) ; Message from John
 #[message 712]
 (alist ((#t . 1.) (#f . 1.))) ; Message to Alarm

 (pp
  (pointwise-sum-product
   (lambda (earthquake burglary alarm)
     (force-assoc
      (list burglary earthquake alarm)
      '(((#t #t #t) . .95)
	((#t #t #f) . 5.0000000000000044e-2)
	((#t #f #t) . .94)
	((#t #f #f) . .06000000000000005)
	((#f #t #t) . .29)
	((#f #t #f) . .71)
	((#f #f #t) . .001)
	((#f #f #f) . .999))))
   (list #t #f)				     ; Earthquake's support
   (make-message '((#t . .001) (#f . .999))) ; Message from burglary
   (make-message '((#t . 1)    (#f . 1)))))  ; Message from alarm
 #[message 818]
 (alist ((#t . 1.) (#f . 1.))) ; Message to earthquake
|#

(propagatify pointwise-sum-product 'no-generic)

(define-propagator-syntax (factor-props factor terminals)
  ;; Ugh.  The factor comes in as a lisp procedure, because 
  ;; currying it the way I want is easier in lisp.
  (define (crunch-the-factor index factor)
    (define (splice lst index value)
      (append
       (take lst index)
       (list value)
       (drop lst index)))
    (define (fixed-factor . values) 
      (let ((other-terminal-values (cdr values))
	    (value-at-index (car values)))
	(apply factor (splice other-terminal-values index value-at-index))))
    (eq-put! fixed-factor 'not-propagator-constructor #t) ;; TODO Fix this!
    (let-cell factor-cell
      (add-content factor-cell fixed-factor)
      factor-cell))
  (for-each
   (lambda (index)
     (let* ((terminal (list-ref terminals index))
	    (other-terminals (delq terminal terminals)))
       (apply p:pointwise-sum-product
	      `(,(crunch-the-factor index factor)
		,(terminal-support terminal)
		,@(map terminal-factor other-terminals)
		,(terminal-variable terminal)))))
   (iota (length terminals))))

(define-structure terminal
  support
  variable
  factor)

;;; Here lives the burglary-earthquake example

(define-structure (node (constructor %make-node))
  support
  marginal
  terminals)

(define (make-node num-terminals)
  (let-cells (support marginal)
    (%make-node
     support marginal
     (map (lambda (index)
	    (let-cells (to-variable to-factor)
	      (make-terminal support to-variable to-factor)))
	  (iota num-terminals)))))

(define (get-terminal node index)
  (list-ref (node-terminals node) index))

(define (variable-at-node node name)
  (diagram-style-with-diagram (empty-diagram name)
    (lambda ()
      (variable (node-support node) (node-marginal node) (node-terminals node)))))

(define (conditional-probability-table alist . terminals)
  (define (normalize-cpt alist)
    ;; This is the place where we assume that the output node is
    ;; Boolean, and the given cpt lists the probability of it being
    ;; true.
    (apply
     append
     (map (lambda (row)
	    (let ((non-output-values (car row))
		  (prob-output-true (cdr row)))
	      `(((,@non-output-values #t) . ,prob-output-true)
		((,@non-output-values #f) . ,(- 1 prob-output-true)))))
	  alist)))
  (let ((nalist (normalize-cpt alist)))
    (define (factor . values)
      (force-assoc values nalist))
    (factor-props factor terminals)))

(define (build-burglary-network)
  (let ((burglary (make-node 2))
	(earthquake (make-node 2))
	(alarm (make-node 3))
	(john-calls (make-node 2))
	(mary-calls (make-node 1)))
    (let ((nodes (list burglary earthquake alarm john-calls mary-calls))
	  (names '(burglary earthquake alarm john-calls mary-calls)))
      (define (boolean-support node)
	(add-content (node-support node) (list #t #f)))
      (for-each boolean-support nodes)
      (for-each variable-at-node nodes names)
      (conditional-probability-table
       '((() . .001))
       (get-terminal burglary 0))
      (conditional-probability-table
       '((() . .002))
       (get-terminal earthquake 0))
      (conditional-probability-table
       '(((#t #t) . .95)
	 ((#t #f) . .94)
	 ((#f #t) . .29)
	 ((#f #f) . .001))
       (get-terminal burglary 1)
       (get-terminal earthquake 1)
       (get-terminal alarm 0))
      (conditional-probability-table
       '(((#t) . .90)
	 ((#f) . .05))
       (get-terminal alarm 1)
       (get-terminal john-calls 0))
      (conditional-probability-table
       '(((#t) . .70)
	 ((#f) . .01))
       (get-terminal alarm 2)
       (get-terminal mary-calls 0))
      ;; Evidence: John called.
      (conditional-probability-table
       '((() . 1.))
       (get-terminal john-calls 1))
      nodes)))

(define (burglary-marginals)
  (initialize-scheduler)
  (let ((nodes (build-burglary-network)))
    (run)
    (for-each pp (map content (map node-marginal nodes)))
    nodes))

#|
 (burglary-marginals)
 #[message 829]
 (alist ((#t . 1.6283729946769937e-2) (#f . .98371627005323)))
 #[message 830]
 (alist ((#t . 1.1394968773811182e-2) (#f . .9886050312261888)))
 #[message 831]
 (alist ((#t . .04343771179992706) (#f . .9565622882000729)))
 #[message 832]
 (alist ((#t . 1.) (#f . 0.)))
 #[message 833]
 (alist ((#t . .03997202114194967) (#f . .9600279788580504)))
|#

#;
(fluid-let ((draw:cell-label
	      (lambda (var) (cons (name var) (if (message? (content var)) (message-alist (content var)) (name (content var)))))))
   (draw:show-graph))
