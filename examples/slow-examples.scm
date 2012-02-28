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

(define-test (small-masyu)
  (interaction
   (do-puzzle
    '("X O  OO   "
      "   O    OO"
      "X XO  O   "
      "   X XO   "
      "OOO  X OX "
      "      X X "
      " X  O    O"
      "     XO   "
      " OOO OXOO "
      "      O   "))
   (produces
    (string-append
     "+ + + + + + + + + + +\n"
     " X---O-\ /-O-O-\ /-\ \n"
     "+|+ + +|+|+ + +|+|+|+\n"
     " |     O \-\ /-/ O O \n"
     "+|+ + +|+ +|+|+ +|+|+\n"
     " X---X O   | O   | | \n"
     "+ + +|+|+ +|+|+ +|+|+\n"
     " /-\ | X---X O   | | \n"
     "+|+|+|+ + + +|+ +|+|+\n"
     " O O O /---X \-O-X | \n"
     "+|+|+|+|+ +|+ + + +|+\n"
     " | | \-/   | X---X | \n"
     "+|+|+ + + +|+|+ +|+|+\n"
     " | X-----O-/ |   | O \n"
     "+|+ + + + + +|+ +|+|+\n"
     " | /-\ /---X O   \-/ \n"
     "+|+|+|+|+ +|+|+ + + +\n"
     " | O O O   O X-O-O-\ \n"
     "+|+|+|+|+ +|+ + + +|+\n"
     " \-/ \-/   \-O-----/ \n"
     "+ + + + + + + + + + +\n"))
   *number-of-calls-to-fail*
   (produces (if *false-premise-starts-out*
		 (if *avoid-false-true-flips* 440 439) 457))
   *worldview-number*
   (produces 2356)))

(define-test (medium-masyu)
  (interaction
   (do-puzzle
    '("   X      O O    X"
      "    O   O   O   X "
      "O        O  O  X  "
      "OO  X XX   X      "
      "      XX  O       "
      "             O   X"
      "X  OOOOX   O   O  "
      "            X  O O"
      "X O O    X      OO"
      "       O    XX    "))
   (produces
    (string-append
     "+ + + + + + + + + + + + + + + + + + +\n"
     "   /---X /-------\ /-O---O-\ /-----X \n"
     "+ +|+ +|+|+ + + +|+|+ + + +|+|+ + +|+\n"
     " /-/   | O   /-\ O \-\ /-O-/ \---X | \n"
     "+|+ + +|+|+ +|+|+|+ +|+|+ + + + +|+|+\n"
     " O /-\ | |   | | \-O-/ \-O-----X | | \n"
     "+|+|+|+|+|+ +|+|+ + + + + + + +|+|+|+\n"
     " O O | | X---X X-----\ X-----\ | | | \n"
     "+|+|+|+|+ + + + + + +|+|+ + +|+|+|+|+\n"
     " \-/ | \-----X X---\ O |   /-/ \-/ | \n"
     "+ + +|+ + + +|+|+ +|+|+|+ +|+ + + +|+\n"
     "     \-\ /-\ | |   \-/ \-\ O /-----X \n"
     "+ + + +|+|+|+|+|+ + + + +|+|+|+ + + +\n"
     " X---\ O O O O X---\ /-O-/ | \-O---\ \n"
     "+|+ +|+|+|+|+|+ + +|+|+ + +|+ + + +|+\n"
     " |   \-/ | | \-\   | \---X | /-O-\ O \n"
     "+|+ + + +|+|+ +|+ +|+ + +|+|+|+ +|+|+\n"
     " X---O-\ O \-\ \---X     | | \-\ O O \n"
     "+ + + +|+|+ +|+ + + + + +|+|+ +|+|+|+\n"
     "       \-/   \-O---------X X---/ \-/ \n"
     "+ + + + + + + + + + + + + + + + + + +\n"))
   *number-of-calls-to-fail*
   (produces (if *false-premise-starts-out*
		 (if *avoid-false-true-flips* 821 930) 855))
   *worldview-number*
   (produces 4522)))

#;
(define-test (large-masyu)
  (interaction
   (do-puzzle
    '("           OX O  OX    X  O    X    "
      "X   O X        X     X        O   X "
      "   X     X  O               X O     "
      "   X         X   X X    XX          "
      "  O    O  XX    X    O    O XX  O  X"
      " O   X       O         O        O  O"
      "          O    X  O     X X    O  O "
      "X   X   X   O O      X  X   X       "
      "        X  O       O XO   O     X X "
      "X  O   O      X O       O      O    "
      "    X    O       O   X        X X X "
      " X O       X  O X  O    OO     O    "
      "      X X O  XO X      X   OX O O   "
      " O X       O         X           X  "
      "  O   XX  X     OX X      X  OO  X  "
      "           O X        XX   O   X    "
      "        O         O  O     O       X"
      "X   O X        X       X        OO  "
      " O X  OO  X     O  O     O   X      "
      "             X      X     X     O O "))
   (produces "???")))

(in-test-group
 riddle-of-the-knights
 (define-test (correct-solution)
   (check
    (generic-match
     `(#(knight sir-sigismund ,s1 ,h4)
       #(knight sir-gerard    ,s2 ,h3)
       #(knight sir-fernando  ,s4 ,h6)
       #(knight sir-harold    ,s7 ,h1)
       #(knight sir-emilio    ,s5 ,h5)
       #(knight sir-almeric   ,s0 ,h7)
       #(knight sir-gawain    ,s3 ,h2)
       #(knight sir-caspar    ,s6 ,h0)
       #(knight sir-jules     ,s8 ,h8)
       #(knight sir-balthus   ,s9 ,h9))
     (map v&s-value (map tms-query (show-time find-solution)))))
   (check (= *number-of-calls-to-fail* 488))
   (check (= *worldview-number* 2410))))

(in-test-group
 albatross-conundrum
 (define-test (correct-solution)
   (let ((answer (map v&s-value (map tms-query (show-time
						find-albatross-solution)))))
     (check
      ;; The puzzle has two consistent assignments, and which one is
      ;; found depends on propagator firing order.
      (boolean/or
       (generic-match
	'(#(deck poop     windlass  galliard-lute        rum)
	  #(deck quarter  scurvy    tamarind-jewels      biscuits)
	  #(deck main     bosun     calypso-figure       firearms)
	  #(deck gun      draconio  casket-of-magenta    ropes)
	  #(deck lower    kraken    goldenhall-talisman  spare-sails))
	answer)
       (generic-match
	'(#(deck poop     windlass  galliard-lute        rum)
	  #(deck quarter  bosun     tamarind-jewels      biscuits)
	  #(deck main     draconio  calypso-figure       firearms)
	  #(deck gun      scurvy    casket-of-magenta    ropes)
	  #(deck lower    kraken    goldenhall-talisman  spare-sails))
	answer))))
   (check (= *number-of-calls-to-fail* 607))
   (check (= *worldview-number* 1858))))

;;; This one is too slow even for the slow-examples!
#;
 (define-test (solve)
   (check (equal?
	   (string-append
	    "327194658\n"
	    "846325179\n"
	    "519687243\n"
	    "172563894\n"
	    "653948721\n"
	    "498712365\n"
	    "764851932\n"
	    "985236417\n"
	    "231479586\n")
	   (with-output-to-string
	     (lambda ()
	       (do-sudoku
		'((0 0 7 0 0 0 6 5 0)
		  (8 4 6 0 0 5 1 0 9)
		  (0 0 9 0 0 0 0 0 3)
		  (1 0 0 5 6 0 0 9 4)
		  (0 0 0 9 4 8 0 0 0)
		  (4 9 0 0 1 2 0 0 5)
		  (7 0 0 0 0 0 9 0 0)
		  (9 0 5 2 0 0 4 1 7)
		  (0 3 1 0 0 0 5 0 0)))))))
   (check (= 629 *number-of-calls-to-fail*)))

