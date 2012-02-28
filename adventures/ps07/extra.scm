;;;; This is the ps07 file extra.scm

;;; These make e: and p: propagators out of the scheme procedures
(propagatify interval-low)
(propagatify interval-high)

(define-propagator (p:in-range? value interval bool)
  (p:and (e:<= (e:interval-low interval) value)
         (e:<= value (e:interval-high interval))
         bool))

(define (add-interval-property estimate interval property-name)
  ;; Is there already such a property on the estimate?
  (let ((status-cell (eq-get estimate property-name))) ;Already defined?
    (if status-cell
	;; Property already exists, get the range cell.
	(let ((range (eq-get estimate (symbol property-name ':range))))
	  (if (not range)
	      (error "Interval property has no range"
		     (name estimate) property-name))
	  (p:== interval range)
	  'range-updated)
	;; New definition:
	;; Create internal cells to hold the status of the symbolic
	;; property and its defining range (initialized to the given interval).
	(let-cells (status-cell range)
	  ;; Initialize the range cell.
	  (p:== interval range)
	  ;; Make the status cell and the range named properties of
	  ;; the estimate cell.
	  (eq-put! estimate (symbol property-name ':range) range)
	  (eq-put! estimate property-name status-cell)
	  ;; If the cell content is within the interval
	  ;; then propagate #t to the status-cell.
	  (p:in-range? estimate range status-cell)
	  ;; If the status is true then propagate the content of the
	  ;; interval-call to the estimate.
	  (p:switch status-cell range estimate)
	  'property-added))))

(define ((c:bins named-ranges) numeric-interval)
  (for-each
   (lambda (named-range)
     (add-interval-property numeric-interval
			    (cadr named-range)
			    (car named-range)))
   named-ranges))


;;; This can be used to support named ranges with a premise
;;; representing the range-defining authority:

(define (named-ranges authority . named-ranges)
  (map (lambda (named-range)
	 (list (car named-range)
	       (depends-on (cadr named-range) authority)))
       named-ranges))
