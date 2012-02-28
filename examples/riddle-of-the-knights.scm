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

(declare (usual-integrations make-cell cell?))

(define-propagator (p:guess-link cell1 cell2)
  (let ((control (e:amb)))
    (p:conditional-wire control cell1 cell2)
    control))

(define-propagator-syntax (quadratic-guess-bijection cells1 cells2)
  (define (not-all-off . cells)
    (require (reduce e:or #f cells)))
  (let ((controls
	 (map (lambda (cell1)
		(map (lambda (cell2)
		       (p:guess-link cell1 cell2))
		     cells2))
	      cells1)))
    ;; I hope that the contents of cells1 and cells2 are forcibly
    ;; distinct, so that connecting one to two will automatically
    ;; fail.
    (for-each (lambda (cell1-controls)
		(apply not-all-off cell1-controls))
	      controls)
    (apply for-each not-all-off controls)))

(define-propagator-syntax (quadratic-extend-bijection cell-alist cells1 cells2)
  (for-each (lambda (cell-pair)
	      (p:id (car cell-pair) (cdr cell-pair))
	      (p:id (cdr cell-pair) (car cell-pair)))
	    cell-alist)
  (quadratic-guess-bijection
   (lset-difference eq? cells1 (map car cell-alist))
   (lset-difference eq? cells2 (map cdr cell-alist))))

(define-structure
  (horse (print-procedure
	  (simple-unparser-method
	   'horse (lambda (horse) (list (horse-name horse))))))
  name color reins plume pattern)

(propagatify horse-color)
(propagatify horse-plume)
(propagatify horse-reins)
(propagatify horse-pattern)

(define-syntax defhorse
  (syntax-rules ()
    ((_ name color reins plume pattern)
     (define name (make-horse 'name 'color 'reins 'plume 'pattern)))))

(defhorse h0 yellow red  white none)
(defhorse h1 brown  red  none  split)
(defhorse h2 white  none none  none)
(defhorse h3 yellow none red   none)
(defhorse h4 black  none red   none)
(defhorse h5 white  none none  none)
(defhorse h6 brown  red  none  arrows)
(defhorse h7 black  red  none  none)
(defhorse h8 gray   none none  crown)
(defhorse h9 white  none none  crown)

(define horses (list h0 h1 h2 h3 h4 h5 h6 h7 h8 h9))

(define (white? horse)
  (eq? (horse-color horse) 'white))

(define (black? horse)
  (eq? (horse-color horse) 'black))

(define (red-reined? horse)
  (eq? (horse-reins horse) 'red))

(define (plumed? horse)
  (not (eq? (horse-plume horse) 'none)))

(propagatify white?)
(propagatify black?)
(propagatify red-reined?)
(propagatify plumed?)

(define-structure
  (shield (print-procedure
	   (simple-unparser-method
	    'shield (lambda (shield) (list (shield-name shield))))))
  name shape pattern)

(propagatify shield-shape)
(propagatify shield-pattern)

(define-syntax defshield
  (syntax-rules ()
    ((_ name shape pattern)
     (define name (make-shield 'name 'shape 'pattern)))))

(defshield s0 square crown)
(defshield s1 square split)
(defshield s2 shield cross)
(defshield s3 shield triangles)
(defshield s4 square arrows)
(defshield s5 shield alt-split)
(defshield s6 shield triangles)
(defshield s7 round  split)
(defshield s8 round  crown)
(defshield s9 round  crown)

(define shields (list s0 s1 s2 s3 s4 s5 s6 s7 s8 s9))

(define knights '(k0 k1 k2 k3 k4 k5 k6 k7 k8 k9))

(define names
  '(sir-gerard sir-almeric sir-jules sir-sigismund sir-balthus
	       sir-fernando sir-caspar sir-gawain sir-harold sir-emilio))

(define-structure
  (knight
   (print-procedure
    (simple-unparser-method
     'knight (lambda (knight)
	       (list (knight-name knight)
		     (knight-shield knight)
		     (knight-horse knight)))))
   (constructor make-knight)
   (constructor make-knight-from-name (name))
   (constructor make-knight-from-shield (shield))
   (constructor make-knight-from-horse (horse)))
  (name nothing)
  (shield nothing)
  (horse nothing))
(declare-type-tester knight? rtd:knight)

(propagatify-monadic knight-name)
(propagatify-monadic knight-shield)
(propagatify-monadic knight-horse)

(propagatify make-knight-from-name)
(propagatify make-knight-from-shield)
(propagatify make-knight-from-horse)

(slotful-information-type knight? make-knight
  knight-name knight-shield knight-horse)

(declare-coercion rtd:knight ->contingent)
(declare-coercion rtd:shield ->contingent)
(declare-coercion rtd:horse ->contingent)

(define (get thing map)
  (let ((cell (assq thing map)))
    (and cell (cdr cell))))

(define (cell-grabber cell-table)
  (lambda (key)
    (or (get key cell-table)
	(error "No owner for" key))))

(define (build-network)
  (let* ((knight-cells
	  (map (lambda (foo) (make-named-cell 'knight)) knights))
	 (knight-name-cells
	  (map e:make-knight-from-name names))
	 (knight-shield-cells
	  (map e:make-knight-from-shield shields))
	 (knight-horse-cells
	  (map e:make-knight-from-horse horses))
	 (cell-table
	  (append
	   (map cons knights knight-cells)
	   (map cons names knight-name-cells)
	   (map cons shields knight-shield-cells)
	   (map cons horses knight-horse-cells)))
	 (cell-of (cell-grabber cell-table))
	 (e:horse-of (lambda (thing)
		       (e:knight-horse (cell-of thing))))
	 (e:shield-of (lambda (thing)
			(e:knight-shield (cell-of thing))))
	 (e:name-of (lambda (thing)
		      (e:knight-name (cell-of thing)))))
    (define (attach-names-to-table! pair)
      (register-diagram (cdr pair) (car pair)))
    (map attach-names-to-table! cell-table)
    ;; This is how the depth-first version of this program specified
    ;; its knights-{shields|horses|names} bijections.
    #|
    (knights-shields
     (extend-distinct-map `((k0 . ,s1) (k1 . ,s2) (k2 . ,s4)
			    (k4 . ,s5) (k7 . ,s6) (k8 . ,s8) (k9 . ,s9))
			  knights shields))
    (knights-horses
     (extend-distinct-map `((k2 . ,h6) (k5 . ,h7) (k8 . ,h8) (k9 . ,h9))
			  knights horses))
    (knights-names
     (extend-distinct-map `((k1 . sir-gerard) (k3 . sir-harold)
			    (k4 . sir-emilio) (k5 . sir-almeric))
			  knights names))
    |#
    #|
    ;; If I specify mine this way, the resulting program takes about
    ;; a day to run, but produces the right answer, and experiences
    ;; approximately 20000 failures.  For reference,
    ;; (map length (map tms-values answers)) produces
    ;; (1461 987 1062 1508 838 1483 3376 2075 1794 3239)
    (quadratic-guess-bijection knight-cells knight-name-cells)
    (quadratic-guess-bijection knight-cells knight-shield-cells)
    (quadratic-guess-bijection knight-cells knight-horse-cells)
    (for-each (lambda (knight shield)
		(require (e:eq? shield (e:shield-of knight))))
	      '(    k0 k1 k2 k4 k7 k8 k9)
	      (list s1 s2 s4 s5 s6 s8 s9))
    (for-each (lambda (knight horse)
		(require (e:eq? horse (e:horse-of knight))))
	      '(    k2 k5 k8 k9)
	      (list h6 h7 h8 h9))
    (for-each (lambda (knight name)
		(require (e:eq? name (e:name-of knight))))
	      '(k1 k3 k4 k5)
	      '(sir-gerard sir-harold sir-emilio sir-almeric))
    |#
    ;; If I specify my bijections this way, the resulting program
    ;; also gets the right answer, but runs much faster:
    ;; ;process time: 885600 (868670 RUN + 16930 GC); real time: 900624
    ;; It also experiences only about 1397 failures, and
    ;; (map length (map tms-values answers)) produces
    ;; (87 16 17 58 17 5 423 83 17 17)
    (define (get-cell-pair key1 key2)
      (cons (get key1 cell-table)
	    (get key2 cell-table)))
    (quadratic-extend-bijection
     (map get-cell-pair
	  '(k1 k3 k4 k5)
	  '(sir-gerard sir-harold sir-emilio sir-almeric))
     knight-cells knight-name-cells)
    (quadratic-extend-bijection
     (map get-cell-pair
	  '(    k0 k1 k2 k4 k7 k8 k9)
	  (list s1 s2 s4 s5 s6 s8 s9))
     knight-cells knight-shield-cells)
    (quadratic-extend-bijection
     (map get-cell-pair
	  '(    k2 k5 k8 k9)
	  (list h6 h7 h8 h9))
     knight-cells knight-horse-cells)
    ;; Document
    (require (e:plumed? (e:horse-of 'sir-gerard)))
    (require (e:not (e:eq? (e:shield-shape (e:shield-of 'sir-almeric))
			   (e:shield-shape (e:shield-of 'sir-jules)))))
    (require (e:eq? (e:shield-pattern (e:shield-of 'sir-almeric))
		    (e:shield-pattern (e:shield-of 'sir-jules))))
					; Embedded
    (require (e:eq? 'cross (e:shield-pattern (e:shield-of 'sir-gerard))))
    (require (e:black? (e:horse-of 'sir-sigismund)))
    (require (e:white? (e:horse-of 'sir-balthus)))
    
    ;; Tapestry
    (require (e:red-reined? (e:horse-of s0)))
    (require (e:eq? h0 (e:horse-of 'sir-caspar)))
    (require (e:eq? h2 (e:horse-of 'sir-gawain)))
    (require (e:white? (e:horse-of s3)))
    (require (e:red-reined? (e:horse-of 'k7)))
    (require (e:red-reined? (e:horse-of s7)))
    
    ;; Horse-pattern
    (for-each (lambda (horse)
		(require (e:or (e:eq? (e:horse-pattern horse) 'none)
			       (e:eq? (e:horse-pattern horse)
				      (e:shield-pattern (e:shield-of horse))))))
	      horses) 
    knight-cells))

(define (find-solution)
  (initialize-scheduler)
  (let ((knights (build-network)))
    (run)
    (map content knights)))

(define-method generic-match ((pattern <vector>) (object rtd:knight))
  (generic-match
   pattern (vector 'knight (knight-name object)
		   (knight-shield object)
		   (knight-horse object))))

#|
 (define answer (show-time find-solution))
 (map v&s-value (map tms-query answer))
 (produces
  `(#(knight sir-sigismund ,s1 ,h4)
    #(knight sir-gerard    ,s2 ,h3)
    #(knight sir-fernando  ,s4 ,h6)
    #(knight sir-harold    ,s7 ,h1)
    #(knight sir-emilio    ,s5 ,h5)
    #(knight sir-almeric   ,s0 ,h7)
    #(knight sir-gawain    ,s3 ,h2)
    #(knight sir-caspar    ,s6 ,h0)
    #(knight sir-jules     ,s8 ,h8)
    #(knight sir-balthus   ,s9 ,h9)))
|#
