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

(in-test-group
 compound-merges
 (define-each-check
   (generic-match (cons nothing nothing) (merge (cons nothing nothing) nothing))
   (generic-match (cons nothing nothing) (merge nothing (cons nothing nothing)))
   (generic-match
    (cons nothing nothing) (merge (cons nothing nothing) (cons nothing nothing)))
   (generic-match (cons 4 nothing) (merge nothing (cons 4 nothing)))
   (generic-match (cons 4 nothing) (merge (cons 4 nothing) nothing))
   (generic-match (cons 4 nothing) (merge (cons 4 nothing) (cons 4 nothing)))
   (generic-match the-contradiction (merge 4 (cons 5 6)))
   (generic-match the-contradiction (merge 4 (cons 4 5)))
   (generic-match the-contradiction (merge 4 (cons nothing nothing)))
   (generic-match '(4 . 5) (merge (cons nothing 5) (cons 4 nothing)))
   (generic-match '(4 . 5) (merge (cons 4 nothing) (cons nothing 5)))
   (generic-match '(4 . 5) (merge (cons 4 5) (cons 4 nothing)))
   (generic-match '(4 . 5) (merge (cons 4 nothing) (cons 4 5)))
   (generic-match '(4 . 5) (merge (cons 4 5) (cons 4 5)))
   ;; This
   #;
   (merge (make-tms (supported (cons (make-tms (supported 4 '(fred))) nothing)
			       '(george)))
	  (make-tms (supported (cons nothing (make-tms (supported 3 '(bill))))
			       '())))
   ;; is mysterious because the result should, I think, look like
   ;; (4:fred,george . 3:bill), but I'm not sure how to make it do
   ;; that.  Also,
   #;
   (merge (make-tms (supported (cons (make-tms (supported 4 '(fred))) nothing)
			       '(george)))
	  (make-tms (supported the-contradiction '(fred george))))
   ;; (or the moral equivalents thereof) should retain the fact that
   ;; george said there was a pair here (in case there's a pair?
   ;; propagator watching), but can probably afford to get rid of the
   ;; 4:fred inside, because if the pair is believed, then george is,
   ;; so fred isn't.
   )

 (define-test (recursive-tms-merge)
   (check
    (generic-match
     #(effectful
       #(tms
	 (#(supported
	    (#(tms (#(supported #(*the-contradiction*) (bill fred))
		    #(supported 4 (fred))
		    #(supported 3 (bill))))
	     .
	     #(*the-nothing*))
	    (george joe))
	  #(supported
	    (#(tms (#(supported 3 (bill)))) . #(*the-nothing*)) (joe))
	  #(supported
	    (#(tms (#(supported 4 (fred)))) . #(*the-nothing*))
	    (george))))
       (#(nogood-effect (joe george bill fred))))
     (merge (make-tms (supported
		       (cons (make-tms (supported 4 '(fred))) nothing)
		       '(george)))
	    (make-tms (supported
		       (cons (make-tms (supported 3 '(bill))) nothing)
		       '(joe)))))))

 (define-test (recursive-tms-merge-2)
   (check
    (generic-match
     #(effectful
       #(tms
	 (#(supported
	    #(kons #(tms (#(supported #(*the-contradiction*) (bill fred))
			  #(supported 4 (fred))
			  #(supported 3 (bill))))
		   #(*the-nothing*))
	    (george joe))
	  #(supported
	    #(kons #(tms (#(supported 3 (bill)))) #(*the-nothing*)) (joe))
	  #(supported
	    #(kons #(tms (#(supported 4 (fred)))) #(*the-nothing*))
	    (george))))
       (#(nogood-effect (joe george bill fred))))
     (merge (make-tms (supported
		       (kons (make-tms (supported 4 '(fred))) nothing)
		       '(george)))
	    (make-tms (supported
		       (kons (make-tms (supported 3 '(bill))) nothing)
		       '(joe)))))))

 )
