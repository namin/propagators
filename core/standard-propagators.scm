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

;;;; Standard primitive propagators

(define (p:constant value)
  (function->propagator-constructor #; (lambda () value)
   (eq-label! (lambda () value) 'name `(constant ,(name value)))))
(define (e:constant value)
  (let ((answer (make-named-cell 'cell)))
    ((constant value) answer)
    (eq-put! answer 'subexprs '())
    answer))

(propagatify abs)
(propagatify square)
(propagatify sqrt)
(propagatify not)
(propagatify negate)
(propagatify invert)
(propagatify sin)
(propagatify cos)
(propagatify tan)
(propagatify asin)
(propagatify acos)
(propagatify exp)
(propagatify log)

(propagatify +)
(propagatify -)
;;;(propagatify *)  ;See below, to make more sophisticated version
;;;(propagatify /)  ;See below, to make more sophisticated version
;;;(propagatify =)  ;See below, to support floating comparisons...
(propagatify <)
(propagatify >)
(propagatify <=)
(propagatify >=)
(propagatify atan2)

; see ../support/utils for num=?
(define generic-= (make-generic-operator 2 '= default-equal?)) 
(define-cell p:=
  (function->propagator-constructor (binary-mapping generic-=)))
(define-cell e:= (expression-style-variant p:=))  

;; Not using propagatify because the name AND names syntax, and I want
;; the procedure BOOLEAN/AND.  Also, see more sophisticated version
;; below.
(define generic-and (make-generic-operator 2 'and boolean/and))
(define-cell p:and-dumb
  (function->propagator-constructor (binary-mapping generic-and)))
(define-cell e:and-dumb (expression-style-variant p:and-dumb))
(define generic-or  (make-generic-operator 2 'or  boolean/or))
(define-cell p:or-dumb
  (function->propagator-constructor (binary-mapping generic-or)))
(define-cell e:or-dumb (expression-style-variant p:or-dumb))

;;; DNA is to AND as division is to multiplication
(define (boolean/dna c x)
  (if (and (not c) x) #f nothing))
(define generic-dna (make-generic-operator 2 'dna boolean/dna))
(define-cell p:dna
  (function->propagator-constructor (binary-mapping generic-dna)))
(define-cell e:dna
  (expression-style-variant p:dna))

(define (boolean/imp a) (if a #t nothing))
(define generic-imp (make-generic-operator 1 'imp boolean/imp))
(define-cell p:imp
  (function->propagator-constructor (unary-mapping generic-imp)))
(define-cell e:imp
  (expression-style-variant p:imp))

;;; RO is to OR as division is to multiplication
(define (boolean/ro c x)
  (if (and c (not x)) #t nothing))
(define generic-ro (make-generic-operator 2 'ro boolean/ro))
(define-cell p:ro
  (function->propagator-constructor (binary-mapping generic-ro)))
(define-cell e:ro
  (expression-style-variant p:ro))

(define (boolean/pmi a) (if (not a) #f nothing))
(define generic-pmi (make-generic-operator 1 'pmi boolean/pmi))
(define-cell p:pmi
  (function->propagator-constructor (unary-mapping generic-pmi)))
(define-cell e:pmi
  (expression-style-variant p:pmi))

(propagatify eq?)
(propagatify eqv?)
(propagatify expt)

;; I want a name for the function that does the switch job
(define (switch control input)
  (if control input nothing))
(define switch-function switch)
(propagatify switch)

(name! identity 'identity)
; These two are almost the same, but the difference doesn't matter
(define-cell p:id (function->propagator-constructor identity))
; (define-cell p:id (function->propagator-constructor (nary-mapping identity)))
(define-cell e:id (expression-style-variant p:id))

(define same identity)
(propagatify same)

;; TODO Do I still want to provide these old names for these things?
(define constant p:constant) (define switch p:switch)

;;;; Standard compound propagators

(define-propagator (conditional control if-true if-false output)
  (switch control if-true output)
  (switch (e:not control) if-false output))

(define-propagator (conditional-router control input if-true if-false)
  (switch control input if-true)
  (switch (e:not control) input if-false))

(define-propagator (conditional-wire control end1 end2)
  (switch control end1 end2)
  (switch control end2 end1))

(define conditional p:conditional)
(define conditional-router p:conditional-router)
(define conditional-wire p:conditional-wire)

;;; Clever Propagators that know about short cuts.

(define-propagator (p:or p1 p2 p)
  (p:or-dumb p1 p2 p)
  ;; Short cuts
  (p:imp p1 p)
  (p:imp p2 p))

(define-propagator (p:and p1 p2 p)
  (p:and-dumb p1 p2 p)
  ;; Short cuts
  (p:pmi p1 p)
  (p:pmi p2 p))


(define generic-* (make-generic-operator 2 '* *))
(define-cell p:*-dumb
  (function->propagator-constructor (binary-mapping generic-*)))
(define-cell e:*-dumb (expression-style-variant p:*-dumb))

(define-propagator (p:* m1 m2 product)
  (p:*-dumb m1 m2 product)
  ;; Short cuts
  (p:switch (e:= m1 0) m1 product)
  (p:switch (e:= m2 0) m2 product))



(define generic-/ (make-generic-operator 2 '/ /))

(define (numerical-zero? x) (and (number? x) (zero? x)))
(declare-explicit-guard numerical-zero? (guard <number> zero?))

(define (binary-nothing a b) nothing)

(defhandler generic-/ binary-nothing numerical-zero? numerical-zero?)

(define (numerical-non-zero? x) (and (number? x) (not (zero? x))))
(declare-explicit-guard numerical-non-zero? (guard <number> numerical-non-zero?))

(define (binary-contradiction a b) the-contradiction)

(defhandler generic-/ binary-contradiction
            numerical-non-zero? numerical-zero?)


(define-cell p:/-dumb
  (function->propagator-constructor (binary-mapping generic-/)))
(define-cell e:/-dumb (expression-style-variant p:/-dumb))

(define-propagator (p:/ product m1 m2)
  (p:/-dumb product m1 m2)
  ;; Short cut
  (p:switch (e:and (e:= product 0)
                   (e:not (e:= m1 0)))
            product m2))

;;; Constraining propagators

(define-propagator (c:+ a1 a2 sum)
  (p:+ a1 a2 sum)      (p:- sum a1 a2)      (p:- sum a2 a1))
;; This generates a useful ce:-
(define-propagator (c:- sum a1 a2)
  (c:+ a1 a2 sum))

(define-propagator (c:* m1 m2 product)
  (p:* m1 m2 product)  (p:/ product m1 m2)  (p:/ product m2 m1))
;; This generates a useful ce:/
(define-propagator (c:/ product m1 m2)
  (c:* m1 m2 product))

(define-propagator (c:square x x^2)
  (p:square x x^2)     (p:sqrt x^2 x))
;; This generates a useful ce:sqrt
(define-propagator (c:sqrt x^2 x)
  (p:square x x^2)     (p:sqrt x^2 x))

(define-propagator (c:not p1 p2)
  (p:not p1 p2)        (p:not p2 p1))

(define-propagator (c:and p1 p2 p)
  (p:and p1 p2 p)
  (p:dna p p1 p2)
  (p:dna p p2 p1)
  (p:imp p p1)
  (p:imp p p2))

(define-propagator (c:or p1 p2 p)
  (p:or p1 p2 p)
  (p:ro p p1 p2)
  (p:ro p p2 p1)
  (p:pmi p p1)
  (p:pmi p p2))

(define-propagator (c:id c1 c2)
  (p:id c1 c2) (p:id c2 c1))

(define-propagator (c:same c1 c2)
  (p:same c1 c2) (p:same c2 c1))

(define-cell p:==
  (propagator-constructor!
   (lambda args
     (let ((target (car (last-pair args))))
       (for-each (lambda (arg)
		   (p:id arg target))
		 (except-last-pair args))
       target))))
(define-cell e:== (expression-style-variant p:==))

(define-cell c:==
  (propagator-constructor!
   (lambda args
     (let ((lead (car args)))
      (for-each (lambda (arg)
		  (c:id lead arg))
		(cdr args))
      lead))))
(define-cell ce:== (expression-style-variant c:==))

(define-propagator (c:negate x y)
  (p:negate x y)
  (p:negate y x))

(define-propagator (c:invert x y)
  (p:invert x y)
  (p:invert y x))

(define-propagator (c:sin x y)
  (p:sin x y)
  (p:asin y x))

(define-propagator (c:cos x y)
  (p:cos x y)
  (p:acos y x))

(define-propagator (c:tan x y)
  (p:tan x y)
  (p:atan2 y 1 x))

(define-propagator (c:exp x y)
  (p:exp x y)
  (p:log y x))

(define-propagator (c:eq? a b truth)
  (p:eq? a b truth)
  (conditional-wire truth a b))

(define-propagator (c:eqv? a b truth)
  (p:eqv? a b truth)
  (conditional-wire truth a b))

