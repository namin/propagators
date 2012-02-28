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

(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../core/load.scm")

(define *virtual-copies* #t)

(define (maybe thing bool)
  (if bool
      (list thing)
      '()))

(for-each load-relative-compiled
 `(,@(maybe "virtual-environments" *virtual-copies*)
   ,@(maybe "virtual-closures" *virtual-copies*)
   "info-alist"
   "functional-reactivity"
   "solve"          ; Requires mechanics to work
   "inequalities"   ; Requires mechanics to work
   "symbolics"      ; Requires mechanics to work
   "symbolics-ineq" ; Requires mechanics to work
   "test-utils"))

(for-each load-relative
 `(,@(maybe "example-closures" *virtual-copies*)
   "draw"
   "dot-writer"
   "graphml-writer"))

(maybe-warn-low-memory)
(initialize-scheduler)
