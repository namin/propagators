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

;;; This is an extended example of using the propagator network system
;;; described in "Propagation Networks", by Alexey Radul.

;;; The problem is to solve Masyu puzzles: see
;;; http://en.wikipedia.org/wiki/Masyu and
;;; http://www.nikoli.com/en/puzzles/masyu/

;;; Each puzzle has a natural external representation as a rectangular
;;; array of characters, each either a space, a capital O (meaning
;;; white circle) or a capital X (meaning black circle).  Load up the
;;; system and this file, and try it out with
#|
 (do-puzzle '(
 "X O  OO   "
 "   O    OO"
 "X XO  O   "
 "   X XO   "
 "OOO  X OX "
 "      X X "
 " X  O    O"
 "     XO   "
 " OOO OXOO "
 "      O   "))

 (do-puzzle '(
 "   X      O O    X"
 "    O   O   O   X "
 "O        O  O  X  "
 "OO  X XX   X      "
 "      XX  O       "
 "             O   X"
 "X  OOOOX   O   O  "
 "            X  O O"
 "X O O    X      OO"
 "       O    XX    "))
|#

;;; The internal representation of an m by n Masyu board is as an
;;; (2m+1) by (2n+1) matrix like this (for 4 by 3):
;;; 
;;; 6   -   -   -   -
;;; 5 |   |   | O |   |
;;; 4   -   -   -   - 
;;; 3 |   |   |   |   |
;;; 2   -   -   -   -
;;; 1 | X |   |   | X |
;;; 0   -   -   -   -
;;;   0 1 2 3 4 5 6 7 8
;;; 
;;; The dashes are boundaries between adjacent squares of the board;
;;; we want to be able to represent them explicitly.  A path on this
;;; board is a bitmap of which boundaries are "on" (part of the path)
;;; and which ones are "off" (not part of the path).  In this example,
;;; the solution is the path (2 5) (4 5) (6 5) (7 4) (7 2) (6 1) (4 1)
;;; (2 1) (1 2) (1 4).
;;; 
;;; We store each bit about whether a boundary is on the path in a
;;; cell of a propagator network, managed by a tms as defined in the
;;; paper.  The dumbest possible search strategy is to just guess
;;; (with binary-amb) whether each boundary is in the path or not,
;;; attach appropriate propagators to enforce all the constraints, and
;;; let the implicit SAT-solver figure it out.  This is what we do
;;; below, and it turns out to work.

;;; Lowlevel board representation

(define-structure board width height cells)

(define (empty-board width height)
  (make-board width height
   (make-initialized-vector width
    (lambda (col)
      (make-initialized-vector height
       (lambda (row)
	 (if (odd? (+ row col))
	     (make-cell)
	     #f)))))))

(define (board-ref board row column)
  (if (and (< -1 column (board-width board))
	   (< -1 row (board-height board)))
      (vector-ref (vector-ref (board-cells board) column) row)
      ;; Outside the board, everything is a cell that is off the path
      (fix-off (make-cell))))

(define (board-set! board row column thing)
  (vector-set! (vector-ref (board-cells board) column) row thing))

;; We can iterate over the indecies that make up the board
(define (each-column board proc)
  (for-each proc (iota (board-width board))) board)

(define (each-row board proc)
  (for-each proc (iota (board-height board))) board)

(define (each-location board proc)
  (each-row board
   (lambda (row)
     (each-column board
      (lambda (col)
	(proc row col))))))

;;; A few utilities for manipulating cells in this context

(define (fix-off cell)
  ((constant (make-tms (list (supported #f '())))) cell)
  cell)

(define (decided? cell)
  (and (not (nothing? (content cell)))
       (not (nothing? (tms-query (content cell))))))

(define (on? cell)
  (and (decided? cell)
       (v&s-value (tms-query (content cell)))))

(define (off? cell)
  (and (decided? cell)
       (not (v&s-value (tms-query (content cell))))))

(define (fail-together v&ss)
  (process-nogood! (apply lset-union eq? (map v&s-support v&ss))))

(define (fail-cells-together cells)
  (fail-together (map tms-query (map content cells))))

;;; Requirements common to every Masyu puzzle

(define (fix-edge! board)
  "The outside boundary of every board cannot be on the path."
  (each-column
   board
   (lambda (col)
     (if (odd? col)
	 (begin
	   (fix-off (board-ref board 0 col))
	   (fix-off (board-ref board (- (board-height board) 1) col))))))
  (each-row
   board
   (lambda (row)
     (if (odd? row)
	 (begin
	   (fix-off (board-ref board row 0))
	   (fix-off (board-ref board row (- (board-width board) 1))))))))

(define (board-space neighbors)
  "A board space is a custom propagator that ensures the continuity
and non-branching of the path."
  (propagator neighbors
    (lambda ()
      (let* ((decided (filter v&s? (map tms-query
					(filter tms? (map content neighbors)))))
	     (on (filter v&s-value decided)))
	;; Can't have three path segments enter one space
	(if (>= (length on) 3)
	    (fail-together (take on 3)))
	;; Can't have exactly one path segment enter a space
	(if (and (= (length on) 1)
		 (= (length decided) 4))
	    (fail-together decided))))))

(define (space-border board row col)
  (list (board-ref board (- row 1) col)
	(board-ref board (+ row 1) col)
	(board-ref board row (- col 1))
	(board-ref board row (+ col 1))))

(define (board-spaces! board)
  "The path must either entirely avoid each space on the board, or
enter and exit that space exactly once; one propagator per space
enforces this."
  (each-location board
   (lambda (row col)
     (if (and (odd? row)
	      (odd? col))
	 (board-space (space-border board row col))))))

(define (guessers! board)
  "To guess whether each cell is on the path, just attach a binary-amb
propagator to it."
  (each-location board
   (lambda (row col)
     (let ((cell (board-ref board row col)))
       (if cell
	   (binary-amb cell))))))

;;; Enforcing circles

;;; It turns out that the constraint imposed by each kind of circle is
;;; separable into two identical pieces, one for the horizontal
;;; neighborhood and one for the vertical.  It also turns out that two
;;; space-boundaries in each direction exactly suffice.  Actually
;;; enforcing these constraints is again done with custom propagators.
(define (white-circle board row col)
  (board-set! board row col #\O)
  (not-all-off (space-border board row col))
  (apply-half-circle half-white-circle board row col))

(define (black-circle board row col)
  (board-set! board row col #\X)
  ;; The not-all-off is implied
  (apply-half-circle half-black-circle board row col))

(define (apply-half-circle half-circle-proc board row col)
  (half-circle-proc (board-ref board (- row 3) col)
		    (board-ref board (- row 1) col)
		    (board-ref board (+ row 1) col)
		    (board-ref board (+ row 3) col))
  (half-circle-proc (board-ref board row (- col 3))
		    (board-ref board row (- col 1))
		    (board-ref board row (+ col 1))
		    (board-ref board row (+ col 3))))

(define (not-all-off cells)
  (propagator cells
    (lambda ()
      (if (every off? cells)
	  (fail-cells-together cells)))))

(define (half-white-circle far-left left right far-right)
  (propagator (list far-left left right far-right)
    (lambda ()
      (if (and (decided? left) (decided? right))
	  (if (or (and (on? left) (off? right))
		  (and (off? left) (on? right)))
	      ;; The path cannot bend at a white circle
	      (fail-cells-together (list left right))
	      (if (and (on? left) (on? right) (on? far-left) (on? far-right))
		  ;; The path has to turn somewhere 
		  (fail-cells-together (list far-left left right far-right))))))))

(define (half-black-circle far-left left right far-right)
  (propagator (list far-left left right far-right)
    (lambda ()
      (if (and (decided? left) (decided? right))
	  (if (or (and (on? left) (on? right))
		  (and (off? left) (off? right)))
	      ;; The path has to bend at a black circle
	      (fail-cells-together (list left right))
	      (if (on? left)
		  ;; The path has to keep straight at each adjacent space
		  (if (off? far-left)
		      (fail-cells-together (list left far-left)))
		  (if (off? far-right)
		      (fail-cells-together (list right far-right)))))))))

;;; The "one loop" constraint

;;; Forcing the path to actually be a single loop, as required by the
;;; problem definition, is a bear, because that constraint is
;;; nonlocal.  Here it is done by a function run outside the network's
;;; scheduler; from the perspective of the network, this comes from
;;; the "user".  The function examines a quiescent board and, if it
;;; finds that the board has multiple loops, issues a failure to the
;;; network and reruns the network.  This is repeated until either the
;;; network decides that it's in a contradictory state or a solution
;;; with a single loop is found.

(define (ensure-correct-looping board)
  (define touched (make-initialized-vector
		   (board-height board)
		   (lambda (row)
		     (make-vector (board-width board) #f))))
  (define (touched? row col)
    (vector-ref (vector-ref touched row) col))
  (define (touch! row col)
    (vector-set! (vector-ref touched row) col #t))
  (define (fresh-start)
    (let loop ((row 0) (col 0))
      (cond ((>= col (board-width board))
	     (loop (+ row 1) 0))
	    ((>= row (board-height board))
	     #f)
	    ((and (not (touched? row col))
		  (procedure? (board-ref board row col))
		  (on? (board-ref board row col)))
	     (cons row col))
	    (else
	     (loop row (+ col 1))))))
  (define (the-loops)
    (let loop ((paths '()))
      (let ((start (fresh-start)))
	(if (not start)
	    paths
	    (loop (cons (loop-from (car start) (cdr start)) paths))))))

  (define (loop-from row col)
    (touch! row col)
    (let loop ((row (if (even? row) (- row 1) row))
	       (col (if (even? row) col (- col 1)))
	       (from (board-ref board row col))
	       (path (list (board-ref board row col))))
      (if (touched? row col)
	  path
	  (begin
	    (touch! row col)
	    ;; There has to be a way to simplify this!
	    (let ((up (board-ref board (- row 1) col))
		  (down (board-ref board (+ row 1) col))
		  (left (board-ref board row (- col 1)))
		  (right (board-ref board row (+ col 1))))
	      (let ((direction (car (filter on? (delq from (list up down left right))))))
		(cond ((eq? direction up)
		       (touch! (- row 1) col)
		       (loop (- row 2) col direction (cons direction path)))
		      ((eq? direction down)
		       (touch! (+ row 1) col)
		       (loop (+ row 2) col direction (cons direction path)))
		      ((eq? direction left)
		       (touch! row (- col 1))
		       (loop row (- col 2) direction (cons direction path)))
		      ((eq? direction right)
		       (touch! row (+ col 1))
		       (loop row (+ col 2) direction (cons direction path))))))))))
  (display "Checking loops in ")
  (display board)
  (newline)
  (print-board board)
  (let ((paths (the-loops)))
    (if (> (length paths) 1)
	(begin
	  (fail-cells-together (car (sort-by paths length)))
	  (let ((run-result (run)))
	    (if (eq? 'done run-result)
		(ensure-correct-looping board)
		(pp run-result))))
	'ok)))

;;; Printing one of these boards is a nice exercise in ASCII art.

(define (print-board board)
  (define (board-character row col)
    (define (corner-mark)
      #\+)
    (define (border-on-mark)
      (if (odd? row) #\- #\|))
    (define (border-mark)
      (let ((cell (board-ref board row col)))
	(let ((content (content cell)))
	  (if (nothing? content)
	      #\?
	      (let ((v&s (tms-query content)))
		(if (nothing? v&s)
		    #\?
		    (if (v&s-value v&s)
			(border-on-mark)
			#\space)))))))
    (define (space-mark)
      (let ((thing (board-ref board row col)))
	(if (char? thing)
	    thing
	    (let ((neighbors (space-border board row col)))
	      (cond ((not (every decided? neighbors))
		     #\space)
		    ((not (= 2 (length (filter on? neighbors))))
		     #\space)
		    (else
		     (path-mark)))))))
    (define (path-mark)
      (let ((up (board-ref board (- row 1) col))
	    (down (board-ref board (+ row 1) col))
	    (left (board-ref board row (- col 1)))
	    (right (board-ref board row (+ col 1))))
	(cond ((and (on? up) (on? down)) #\|)
	      ((and (on? left) (on? right)) #\-)
	      ((and (on? up) (on? right)) #\\)
	      ((and (on? up) (on? left)) #\/)
	      ((and (on? down) (on? right)) #\/)
	      ((and (on? down) (on? left)) #\\))))
    (cond ((and (even? row) (even? col))
	   (corner-mark))
	  ((or (even? row) (even? col))
	   (border-mark))
	  (else
	   (space-mark))))
  (each-row board
   (lambda (row)
     (each-column board
      (lambda (col)
	(write-char (board-character row col))))
     (newline))))

;;; Toplevel interface

(define (do-puzzle puzzle)
  "Solves a Masyu puzzle given as a vector of strings of spaces, O's,
and X's by instantiating and running a propagator network (and
externally enforcing the one-loop constraint)."
  (show-time
   (lambda ()
     (initialize-scheduler)
     (let ((board (parse-puzzle puzzle)))
       (let ((run-result (run)))
	 (if (eq? 'done run-result)
	     (ensure-correct-looping board)
	     (pp run-result)))
       (display "Finished ")
       (display board)
       (newline)
       (print-board board)))))

(define (parse-puzzle strings)
  "Parses a vector of strings of O's, X's, and spaces into a freshly
allocated board, and instantiates (but does not run) a propagator
network to solve it."
  (if (not (apply = (map string-length strings)))
      (error "The puzzle is not rectangluar" strings))
  (let* ((apparent-height (length strings))
	 (apparent-width (string-length (car strings)))
	 (base-board
	  (empty-board (+ (* 2 apparent-width) 1)
		       (+ (* 2 apparent-height) 1))))
    (fix-edge! base-board)
    (board-spaces! base-board)
    (guessers! base-board)
    (let per-string ((strings strings) (row 1))
      (if (null? strings)
	  base-board
	  (let per-char ((chars (string->list (car strings)))
			 (column 1))
	    (if (null? chars)
		(per-string (cdr strings) (+ row 2))
		(begin 
		  (if (eq? (car chars) #\X)
		      (black-circle base-board row column))
		  (if (eq? (car chars) #\O)
		      (white-circle base-board row column))
		  (per-char (cdr chars) (+ column 2)))))))))

(define-method generic-match ((pattern <string>) (object rtd:board))
  (define (fix-masyu-string string)
    (string-replace
     (string-replace string "- " "-\\ ")
     " -" " \\-"))
  (equal? (fix-masyu-string pattern)
	  (with-output-to-string (lambda () (print-board object)))))
