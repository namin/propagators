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
 dependencies

 (define-test (smoke)
   (interaction
    (initialize-scheduler)
    (define-cell frob (make-tms (contingent 4 '(bill))))
    (define-cell maybe-frob (e:switch (make-tms (contingent #t '(fred))) frob))
    (run)
    (tms-query (content maybe-frob))
    (produces #(supported 4 (bill fred)))))

 (define-test (supported-barometer)
   (interaction
    (initialize-scheduler)
    (define-cell barometer-height)
    (define-cell barometer-shadow)
    (define-cell building-height)
    (define-cell building-shadow)
    (c:similar-triangles barometer-shadow barometer-height
			 building-shadow building-height)

    (add-content building-shadow
		 (supported (make-interval 54.9 55.1) '(shadows)))
    (add-content barometer-height
		 (supported (make-interval 0.3 0.32) '(shadows)))
    (add-content barometer-shadow
		 (supported (make-interval 0.36 0.37) '(shadows)))
    (run)
    (content building-height)
    (produces #(supported #(interval 44.514 48.978) (shadows)))

    (define-cell fall-time)
    (c:fall-duration fall-time building-height)

    (add-content fall-time
		 (supported (make-interval 2.9 3.3) '(lousy-fall-time)))
    (run)
    (content building-height)
    (produces #(supported #(interval 44.514 48.978) (shadows)))

    (add-content fall-time
		 (supported (make-interval 2.9 3.1) '(better-fall-time)))
    (run)
    (content building-height)
    (produces #(supported #(interval 44.514 47.243)
			  (better-fall-time shadows)))

    (add-content building-height (supported 45 '(superintendent)))
    (run)
    (content building-height)
    (produces #(supported 45 (superintendent)))

    (content barometer-height)
    (produces #(supported #(interval .3 .30328)
			  (superintendent better-fall-time shadows)))

    (content barometer-shadow)
    (produces #(supported #(interval .366 .37)
			  (better-fall-time superintendent shadows)))

    (content building-shadow)
    (produces #(supported #(interval 54.9 55.1) (shadows)))

    (content fall-time)
    (produces #(supported #(interval 3.0255 3.0322) (superintendent)))
    ))

 (define-test (tms-barometer)
   (interaction
    (initialize-scheduler)
    (define-cell barometer-height)
    (define-cell barometer-shadow)
    (define-cell building-height)
    (define-cell building-shadow)
    (c:similar-triangles barometer-shadow barometer-height
			 building-shadow building-height)

    (add-content building-shadow
		 (make-tms (supported (make-interval 54.9 55.1) '(shadows))))
    (add-content barometer-height
		 (make-tms (supported (make-interval 0.3 0.32) '(shadows))))
    (add-content barometer-shadow
		 (make-tms (supported (make-interval 0.36 0.37) '(shadows))))
    (run)
    (content building-height)
    (produces #(tms (#(supported #(interval 44.514 48.978) (shadows)))))

    (define-cell fall-time)
    (c:fall-duration fall-time building-height)

    (add-content fall-time
		 (make-tms (supported (make-interval 2.9 3.1) '(fall-time))))
    (run)
    (content building-height)
    (produces #(tms (#(supported #(interval 44.514 47.243)
				 (fall-time shadows))
		     #(supported #(interval 44.514 48.978)
				 (shadows)))))

    (tms-query (content building-height))
    (produces #(supported #(interval 44.514 47.243) (fall-time shadows)))

    (kick-out! 'fall-time)
    (run)
    (tms-query (content building-height))
    (produces #(supported #(interval 44.514 48.978) (shadows)))

    (kick-out! 'shadows)
    (run)
    (tms-query (content building-height))
    (produces #(*the-nothing*))

    (bring-in! 'fall-time)
    (run)
    (tms-query (content building-height))
    (produces #(supported #(interval 41.163 47.243) (fall-time)))

    (content building-height)
    (produces #(tms (#(supported #(interval 41.163 47.243)
				 (fall-time))
		     #(supported #(interval 44.514 47.243)
				 (fall-time shadows))
		     #(supported #(interval 44.514 48.978)
				 (shadows)))))

    (add-content building-height (supported 45 '(superintendent)))

    (run)
    (content building-height)
    (produces #(tms (#(supported 45 (superintendent))
		     #(supported #(interval 41.163 47.243)
				 (fall-time))
		     #(supported #(interval 44.514 47.243)
				 (fall-time shadows))
		     #(supported #(interval 44.514 48.978)
				 (shadows)))))

    (tms-query (content building-height))
    (produces #(supported 45 (superintendent)))

    (bring-in! 'shadows)
    (run)
    (tms-query (content building-height))
    (produces #(supported 45 (superintendent)))

    (content barometer-height)
    (produces #(tms (#(supported #(interval .3 .30328)
				 (fall-time superintendent shadows))
		     #(supported #(interval .29401 .30328)
				 (superintendent shadows))
		     #(supported #(interval .3 .31839)
				 (fall-time shadows))
		     #(supported #(interval .3 .32) (shadows)))))


    (tms-query (content barometer-height))
    (produces #(supported #(interval .3 .30328)
			  (fall-time superintendent shadows)))

    (kick-out! 'fall-time)
    (run)
    (tms-query (content barometer-height))
    (produces #(supported #(interval .3 .30328) (superintendent shadows)))

    (bring-in! 'fall-time)
    (run)
    (tms-query (content barometer-height))
    (produces #(supported #(interval .3 .30328) (superintendent shadows)))

    (content barometer-height)
    (produces #(tms (#(supported #(interval .3 .30328)
				 (superintendent shadows))
		     #(supported #(interval .3 .31839)
				 (fall-time shadows))
		     #(supported #(interval .3 .32) (shadows)))))
    ))

 (define-test (contradictory-barometer)
   (interaction
    ;; Restore the state we had in the preceding example
    (initialize-scheduler)
    (define-cell barometer-height)
    (define-cell barometer-shadow)
    (define-cell building-height)
    (define-cell building-shadow)
    (c:similar-triangles barometer-shadow barometer-height
			 building-shadow building-height)

    (add-content building-shadow
		 (make-tms (supported (make-interval 54.9 55.1) '(shadows))))
    (add-content barometer-height
		 (make-tms (supported (make-interval 0.3 0.32) '(shadows))))
    (add-content barometer-shadow
		 (make-tms (supported (make-interval 0.36 0.37) '(shadows))))

    (define-cell fall-time)
    (c:fall-duration fall-time building-height)

    (add-content fall-time
		 (make-tms (supported (make-interval 2.9 3.1) '(fall-time))))
    (run)
    (tms-query (content building-height))

    (kick-out! 'fall-time)
    (run)
    (tms-query (content building-height))

    (bring-in! 'fall-time)
    (kick-out! 'shadows)
    (run)
    (tms-query (content building-height))

    (add-content building-height (supported 45 '(superintendent)))
    (run)
    (bring-in! 'shadows)
    (run)
    (tms-query (content building-height))

    (tms-query (content barometer-height))
    (kick-out! 'fall-time)
    (run)
    (tms-query (content barometer-height))
    (bring-in! 'fall-time)
    (run)
    (tms-query (content barometer-height))
    ;; That was a long story!

    (add-content building-height
		 (supported (make-interval 46. 50.) '(pressure)))
    (run)
    (produces '(contradiction (superintendent pressure)))

    (tms-query (content building-height))
    (produces #(supported #(interval 46. 45) (superintendent pressure)))

    (tms-query (content barometer-height))
    (produces #(supported #(interval .3 .30328) (superintendent shadows)))

    (kick-out! 'superintendent)
    (run)
    (tms-query (content building-height))
    (produces #(supported #(interval 46. 47.243)
			  (shadows fall-time pressure)))

    (tms-query (content barometer-height))
    (produces #(supported #(interval .30054 .31839)
			  (pressure fall-time shadows)))

    (bring-in! 'superintendent)
    (kick-out! 'pressure)
    (run)
    (tms-query (content building-height))
    (produces #(supported 45 (superintendent)))
    (tms-query (content barometer-height))
    (produces #(supported #(interval .3 .30328) (superintendent shadows)))
    ))

 )
