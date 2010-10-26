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


;;;;           Most General Generic-Operator Dispatch

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION).

;;; To be the correct branch all arguments must be accepted by
;;; the branch predicates, so this makes it necessary to
;;; backtrack to find another branch where the first argument
;;; is accepted if the second argument is rejected.  Here
;;; backtracking is implemented by OR.

(define (make-generic-operator arity #!optional name default-operation)
  (guarantee-exact-positive-integer arity 'make-generic-operator)
  (if (not (fix:fixnum? arity))
      (error:bad-range-argument arity 'make-generic-operator))
  (if (not (default-object? name))
      (guarantee-symbol name 'make-generic-operator))
  (if (not (default-object? default-operation))
      (guarantee-procedure-of-arity
       default-operation arity 'make-generic-operator))
  (let ((record (make-operator-record arity)))
    (define operator
      (case arity
        ((1)
         (lambda (arg)
           ((or (find-branch (operator-record-tree record) arg win-handler)
                default-operation)
            arg)))
        ((2)
         (lambda (arg1 arg2)
           ((or (find-branch (operator-record-tree record) arg1
                             (lambda (branch)
                               (find-branch branch arg2 win-handler)))
                default-operation)
            arg1
            arg2)))
        (else
         (lambda arguments
           (if (not (fix:= (length arguments) arity))
               (error:wrong-number-of-arguments operator arity arguments))
           (apply (or (let loop ((tree (operator-record-tree record))
                                 (args arguments))
                        (find-branch tree (car args)
                                     (if (pair? (cdr args))
                                         (lambda (branch)
                                           (loop branch (cdr args)))
                                         win-handler)))
                      default-operation
		      (error:no-applicable-methods operator name arguments))
                  arguments)))))
    (define (find-branch tree arg win)
      (let loop ((tree tree))
        (and (pair? tree)
             (or (and ((caar tree) arg) (win (cdar tree)))
                 (loop (cdr tree))))))
    (define (win-handler handler) handler)
    (set! default-operation
      (if (default-object? default-operation)
          (lambda arguments (no-way-known operator name arguments))
          default-operation))
    (set-operator-record! operator record)
    ;; For backwards compatibility with previous implementation:
    (if (not (default-object? name))
        (set-operator-record! name record))
    operator))

(define *generic-operator-table*
  (make-eq-hash-table))

(define (get-operator-record operator)
  (hash-table/get *generic-operator-table* operator #f))

(define (set-operator-record! operator record)
  (hash-table/put! *generic-operator-table* operator record))

(define (make-operator-record arity) (cons arity '()))
(define (operator-record-arity record) (car record))
(define (operator-record-tree record) (cdr record))
(define (set-operator-record-tree! record tree) (set-cdr! record tree))

(define (generic-operator-arity operator)
  (let ((record (get-operator-record operator)))
    (if record
        (operator-record-arity record)
        (error "Not an operator:" operator))))

(define (assign-operation operator handler . argument-predicates)
  (let ((record
         (let ((record (get-operator-record operator))
               (arity (length argument-predicates)))
           (if record
               (begin
                 (if (not (fix:= arity (operator-record-arity record)))
                     (error "Incorrect operator arity:" operator))
                 record)
               (let ((record (make-operator-record arity)))
                 (set-operator-record! operator record)
                 record)))))
    (set-operator-record-tree! record
      (bind-in-tree argument-predicates
		    handler
		    (operator-record-tree record))))
  operator)

(define defhandler assign-operation)

(define (bind-in-tree keys handler tree)
  (let loop ((keys keys) (tree tree))
    (let ((p.v (assq (car keys) tree)))
      (if (pair? (cdr keys))
          (if p.v
              (begin
                (set-cdr! p.v
                          (loop (cdr keys) (cdr p.v)))
                tree)
              (cons (cons (car keys)
                          (loop (cdr keys) '()))
                    tree))
          (if p.v
              (begin
                (warn "Replacing a handler:" (cdr p.v) handler)
                (set-cdr! p.v handler)
                tree)
              (cons (cons (car keys) handler)
                    tree))))))
                                    
(define (no-way-known operator name arguments)
  (error "Generic operator inapplicable:" operator name arguments))

;;; Utility
(define (any? x)
  #t)
