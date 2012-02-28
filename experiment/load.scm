(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))


(load-relative "../support/load")

(cd "../core")
(for-each load
  '("scheduler"
    "metadata"
    "merge-effects"
    "cells"
    "cell-sugar"
    "propagators"
    "application"
    "sugar"
    "generic-definitions"
    "compound-data"
    "physical-closures"
    "standard-propagators"
    "carrying-cells"

    ;;Intervals must follow standard-propagators in the load order
    ;;because it depends on interval-non-zero?, numerical-zero?,
    ;;binary-nothing, and binary-contradiction previously defined.
   
    "intervals"
    "premises"))

(cd "../experiment")
(for-each load
  '("depends"
    "lash-up"))

(cd "../core")
(for-each load
  '("truth-maintenance"
    "contradictions"
    "search"
    "amb-utils"

    "example-networks"
    "test-utils"))

(define *virtual-copies* #t)

(define (maybe thing bool)
  (if bool
      (list thing)
      '()))


(cd "../extensions")
(for-each load
 `(,@(maybe "virtual-environments" *virtual-copies*)
   ,@(maybe "virtual-closures" *virtual-copies*)
   "info-alist"
   "functional-reactivity"
   "solve"          ; Requires mechanics to work
   "inequalities"   ; Requires mechanics to work
   "symbolics"      ; Requires mechanics to work
   "symbolics-ineq" ; Requires mechanics to work
   "test-utils"))

(for-each load
 `(,@(maybe "example-closures" *virtual-copies*)
   "draw"
   "dot-writer"
   "graphml-writer"))

(maybe-warn-low-memory)
(initialize-scheduler)
