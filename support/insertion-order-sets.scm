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

;;;; Sets that preserve insertion order

(define-structure (insertion-order-set (conc-name oset-))
  table
  list)

(define (make-eq-oset)
  (make-insertion-order-set (make-strong-eq-hash-table) '()))

;; Turning this off makes the order in which propagators are run vary
;; chaotically.  That is not supposed to cause trouble in principle,
;; but a reproducible run order can be valuable for debugging the
;; infrastructure.  The chaotic variation also causes variations in the 
;; *number-of-calls-to-fail* when doing dependency directed backtracking.
(define *reproducible-order* #t)

(define (oset-insert oset thing)
  (hash-table/lookup
   (oset-table oset)
   thing
   (lambda (value) 'ok)
   (lambda ()
     (hash-table/put! (oset-table oset) thing #t)
     (set-oset-list! oset (cons thing (oset-list oset))))))

(define (oset-peek oset)
  (if (= 0 (oset-count oset))
      (error "Peeking empty oset" oset))
  (if *reproducible-order*
      (car (oset-list oset))
      (car (hash-table/key-list (oset-table oset)))))

(define (oset-pop! oset)
  (let ((answer (oset-peek oset)))
    (hash-table/remove! (oset-table oset) answer)
    (set-oset-list! oset (cdr (oset-list oset)))
    answer))

(define (oset-members oset)
  (if *reproducible-order*
      (list-copy (oset-list oset))
      (hash-table/key-list (oset-table oset))))

(define (oset-clear! oset)
  (hash-table/clear! (oset-table oset))
  (set-oset-list! oset '()))

(define (oset-count oset)
  (hash-table/count (oset-table oset)))
