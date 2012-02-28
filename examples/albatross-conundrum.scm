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

(define-structure
  (deck
   (print-procedure
    (simple-unparser-method
     'deck (lambda (deck)
	     (list (deck-name deck)
		   (deck-commander deck)
		   (deck-treasure deck)
		   (deck-supply deck)))))
   (constructor make-deck)
   (constructor make-deck-from-name (name))
   (constructor make-deck-from-commander (commander))
   (constructor make-deck-from-treasure (treasure))
   (constructor make-deck-from-supply (supply)))
  (name nothing)
  (commander nothing)
  (treasure nothing)
  (supply nothing))
(declare-type-tester deck? rtd:deck)

(propagatify-monadic deck-name)
(propagatify-monadic deck-commander)
(propagatify-monadic deck-treasure)
(propagatify-monadic deck-supply)

(propagatify make-deck-from-name)
(propagatify make-deck-from-commander)
(propagatify make-deck-from-treasure)
(propagatify make-deck-from-supply)

(declare-coercion rtd:deck ->contingent)

(slotful-information-type deck? make-deck
  deck-name deck-commander deck-treasure deck-supply)

(define deck-names '(poop quarter main gun lower))
(define commanders '(draconio bosun scurvy kraken windlass))
(define treasures
  '(casket-of-magenta tamarind-jewels galliard-lute calypso-figure goldenhall-talisman))
(define supplies '(ropes spare-sails rum biscuits firearms))

(define (build-albatross-network)
  (let* ((deck-cells
	  (map e:make-deck-from-name deck-names))
	 (deck-commander-cells
	  (map e:make-deck-from-commander commanders))
	 (deck-treasure-cells
	  (map e:make-deck-from-treasure treasures))
	 (deck-supply-cells
	  (map e:make-deck-from-supply supplies))
	 (cell-table
	  (append
	   (map cons deck-names deck-cells)
	   (map cons commanders deck-commander-cells)
	   (map cons treasures deck-treasure-cells)
	   (map cons supplies deck-supply-cells)))
	 (cell-of (cell-grabber cell-table))
	 (e:deck-name-of (lambda (thing)
			   (e:deck-name (cell-of thing))))
	 (e:commander-of (lambda (thing)
			   (e:deck-commander (cell-of thing))))
	 (e:treasure-of (lambda (thing)
			  (e:deck-treasure (cell-of thing))))
	 (e:supply-of (lambda (thing)
			(e:deck-supply (cell-of thing)))))
    (define (attach-names-to-table! pair)
      (register-diagram (cdr pair) (car pair)))
    (map attach-names-to-table! cell-table)
    (quadratic-guess-bijection deck-cells deck-commander-cells)
    (quadratic-guess-bijection deck-cells deck-treasure-cells)
    (quadratic-guess-bijection deck-cells deck-supply-cells)

    (require (e:eq? 'gun (e:deck-name-of 'casket-of-magenta)))
    (require (e:or (e:eq? 'casket-of-magenta (e:treasure-of 'scurvy))
		   (e:eq? 'tamarind-jewels (e:treasure-of 'scurvy))))
    (require (e:not (e:eq? 'gun (e:deck-name-of 'windlass))))
    (require (e:not (e:eq? 'gun (e:deck-name-of 'bosun))))
    (require (e:not (e:eq? 'lower (e:deck-name-of 'galliard-lute))))
    (require (e:not (e:eq? 'poop (e:deck-name-of 'spare-sails))))
    (require (e:not (e:eq? 'main (e:deck-name-of 'windlass))))
    (require (e:not (e:eq? 'goldenhall-talisman (e:treasure-of 'bosun))))
    (require (e:not (e:eq? 'galliard-lute (e:treasure-of 'bosun))))
    (require (e:not (e:eq? 'goldenhall-talisman (e:treasure-of 'draconio))))
    (require (e:not (e:eq? 'galliard-lute (e:treasure-of 'draconio))))
    (require (e:not (e:eq? 'biscuits (e:supply-of 'kraken))))
    (require (e:not (e:eq? 'firearms (e:supply-of 'kraken))))

    (require (e:not (e:eq? 'quarter (e:deck-name-of 'galliard-lute))))
    (require (e:not (e:eq? 'quarter (e:deck-name-of 'calypso-figure))))
    (require (e:not (e:eq? 'quarter (e:deck-name-of 'spare-sails))))
    (require (e:not (e:eq? 'kraken (e:commander-of 'casket-of-magenta))))
    (require (e:not (e:eq? 'windlass (e:commander-of 'spare-sails))))
    (require (e:not (e:eq? 'windlass (e:commander-of 'biscuits))))
    (require (e:eq? 'main (e:deck-name-of 'firearms)))
    (require (e:not (e:eq? 'draconio (e:commander-of 'spare-sails))))
    (require (e:not (e:eq? 'draconio (e:commander-of 'biscuits))))
    (require (e:not (e:eq? 'rum (e:supply-of 'casket-of-magenta))))
    (require (e:not (e:eq? 'scurvy (e:commander-of 'spare-sails))))
    (require (e:not (e:eq? 'biscuits (e:supply-of 'casket-of-magenta))))
    (require (e:not (e:eq? 'biscuits (e:supply-of 'goldenhall-talisman))))

    deck-cells))

(define (find-albatross-solution)
  (initialize-scheduler)
  (let ((decks (build-albatross-network)))
    (run)
    (map content decks)))

(define-method generic-match ((pattern <vector>) (object rtd:deck))
  (generic-match
   pattern (vector 'deck (deck-name object)
		   (deck-commander object)
		   (deck-treasure object)
		   (deck-supply object))))

#|
 (define answer (show-time find-albatross-solution))
 (map v&s-value (map tms-query answer))
 (produces
  '(#(deck poop     windlass  galliard-lute        rum)
    #(deck quarter  bosun     tamarind-jewels      biscuits)
    #(deck main     draconio  calypso-figure       firearms)
    #(deck gun      scurvy    casket-of-magenta    ropes)
    #(deck lower    kraken    goldenhall-talisman  spare-sails)))
;; or
  '(#(deck poop     windlass  galliard-lute        rum)
    #(deck quarter  scurvy    tamarind-jewels      biscuits)
    #(deck main     bosun     calypso-figure       firearms)
    #(deck gun      draconio  casket-of-magenta    ropes)
    #(deck lower    kraken    goldenhall-talisman  spare-sails))
|#
