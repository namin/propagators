;;;; This is the ps07 file ui.scm

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)

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

;;; This abstracts an annoying composition
(define (depends-on information . premises)
  (make-tms (contingent information premises)))

;;; This is required because (run) returns old value if there is
;;; nothing to do.  This is a problem if a contradiction is resolved
;;; by a kick-out! with no propagation.

(define (tell! cell information . informants)
  (assert (cell? cell) "Can only tell something to a cell.")
  (set! *last-value-of-run* 'done)
  (add-content cell (make-tms (contingent information informants)))
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
  (let ((c (content cell)))
    (if (tms? c)
	(let ((v (tms-query c)))
	  (cond ((nothing? v) v)
		((contingent? v) v)
		(else
		 (error "Bug: TMS contains non-contingent statement"
			cell))))
	c)))

