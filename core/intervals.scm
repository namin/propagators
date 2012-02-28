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

#|
(define-structure
  (%interval (safe-accessors #t)
	     (print-procedure
	      (simple-unparser-method
	       'interval
	       (lambda (interval)
		 (list (interval-low interval)
		       (interval-high interval))))))
  low high)
|#

(define (interval-printer state object)
  (if (empty-interval? object)
      (with-current-unparser-state state
	(lambda (port)
	  (display "#[contradictory-interval " port)
	  (write (interval-low object) port)
	  (display " " port)
	  (write (interval-high object) port)
	  (display "]" port)))
      (with-current-unparser-state state
	(lambda (port)
	  (display "#[interval " port)
	  (write (interval-low object) port)
	  (display " " port)
	  (write (interval-high object) port)
	  (display "]" port)))))

(define-structure
  (%interval (safe-accessors #t)
	     (print-procedure interval-printer))
  low high)

(declare-type-tester %interval? rtd:%interval)
(declare-coercion-target %interval)
(declare-coercion <number> ->%interval (lambda (x) (make-%interval x x)))

(define (%interval-> int)
  (if (= (interval-low int) (interval-high int))
      (interval-low int)
      int))

(define (interval? x)
  (or (%interval? x)
      (%interval-able? x)))

(define (interval-low thing)
  (%interval-low (->%interval thing)))

(define (interval-high thing)
  (%interval-high (->%interval thing)))

(define (make-interval low high)
  (%interval-> (make-%interval low high)))


(define (interval-equal? int1 int2)
  (and (= (interval-low int1) (interval-low int2))
       (= (interval-high int1) (interval-high int2))))

(define (add-interval x y)
  (make-interval (+ (interval-low x) (interval-low y))
		 (+ (interval-high x) (interval-high y))))

(define (sub-interval x y)
  (make-interval (- (interval-low x) (interval-high y))
		 (- (interval-high x) (interval-low y))))

(define (mul-interval x y)
  (let ((p1 (* (interval-low x)  (interval-low y)))
	(p2 (* (interval-low x)  (interval-high y)))
	(p3 (* (interval-high x) (interval-low y)))
	(p4 (* (interval-high x) (interval-high y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define *error-on-zero-interval-division* #f)

(define (div-interval x y)
  (if (<= (interval-low y) 0 (interval-high y))
      (if *error-on-zero-interval-division*
	  (error "Cannot divide by interval spanning 0" x y)
	  nothing)
      (mul-interval x
		    (make-interval (/ 1 (interval-high y))
				   (/ 1 (interval-low y))))))

(define (square-interval x)
  (make-interval (square (interval-low x))
                 (square (interval-high x))))

(define (sqrt-interval x)
  (make-interval (sqrt (interval-low x))
                 (sqrt (interval-high x))))

(define (log-interval x)
  (make-interval (log (interval-low x))
                 (log (interval-high x))))

(define (exp-interval x)
  (make-interval (exp (interval-low x))
                 (exp (interval-high x))))

(define (empty-interval? x)
  (> (interval-low x) (interval-high x)))

#| 
;;; This makes a floating-point disaster...  UGH!
;;; MERGE does not consider intervals differing by roundoff to be EQUAL.

(define (intersect-intervals x y)
  (define (exactness-max x y)
    (if (>= x y) x y))
  (define (exactness-min x y)
    (if (<= x y) x y))
  (make-interval
   (exactness-max (interval-low x) (interval-low y))
   (exactness-min (interval-high x) (interval-high y))))
|#

(define (intersect-intervals x y)
  (define (exactness-max x y)
    (if (or (num=? x y) (> x y)) x y))
  (define (exactness-min x y)
    (if (or (num=? x y) (< x y)) x y))
  (make-interval
   (exactness-max (interval-low x) (interval-low y))
   (exactness-min (interval-high x) (interval-high y))))

;; This differs from interval-equal? because is has to be monotonic
;; with respect to the intervals shrinking.
(define (=-interval x y)
  (or (= (interval-low x) (interval-high x)
         (interval-low y) (interval-high y))
      (and (not (empty-interval? (intersect-intervals x y)))
           nothing)))

(define (<-interval x y)
  (or (< (interval-high x) (interval-low y))
      (and (< (interval-low x) (interval-high y))
	   nothing)))

(define (<=-interval x y)
  (or (<= (interval-high x) (interval-low y))
      (and (<= (interval-low x) (interval-high y))
	   nothing)))

(define (>-interval x y)
  (or (> (interval-low x) (interval-high y))
      (and (> (interval-high x) (interval-low y))
	   nothing)))

(define (>=-interval x y)
  (or (>= (interval-low x) (interval-high y))
      (and (>= (interval-high x) (interval-low y))
	   nothing)))

(defhandler-coercing generic-+ add-interval ->%interval)
(defhandler-coercing generic-- sub-interval ->%interval)
(defhandler-coercing generic-* mul-interval ->%interval)
(defhandler-coercing generic-/ div-interval ->%interval)

(defhandler-coercing generic-= =-interval ->%interval)
(defhandler-coercing generic-< <-interval ->%interval)
(defhandler-coercing generic-<= <=-interval ->%interval)
(defhandler-coercing generic-> >-interval ->%interval)
(defhandler-coercing generic->= >=-interval ->%interval)

(defhandler generic-square square-interval %interval?)
(defhandler generic-sqrt sqrt-interval %interval?)

(defhandler generic-log log-interval %interval?)
(defhandler generic-exp exp-interval %interval?)

(defhandler-coercing merge intersect-intervals ->%interval)
(defhandler-coercing equivalent? interval-equal? ->%interval)

(defhandler contradictory? empty-interval? %interval?)


(define (interval-non-zero? x)
  (and (interval? x)
       (or (< 0 (interval-low x)) (> 0 (interval-high x)))))
(declare-explicit-guard interval-non-zero? (guard rtd:%interval interval-non-zero?))

(defhandler generic-/ binary-contradiction
            interval-non-zero? numerical-zero?)

(define (interval-contains-zero? x)
  (and (interval? x)
       (and (<= (interval-low x) 0) (>= (interval-high x) 0))))
(declare-explicit-guard interval-contains-zero? (guard rtd:%interval interval-contains-zero?))
  
(defhandler generic-/ binary-nothing
            interval-contains-zero? numerical-zero?)


