;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul.
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
 selectors
 
 (define-test (walk-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (add-content go? 'go-deep)
    (define-cell walk-to-met)
    (add-content walk-to-met (make-trip-segment-key 'start 'home 'end 'met))
    ((force plan-walk) go? walk-to-met)
    (run)
    (check (with-units? (trip-segment-time (content walk-to-met))))
    (check (eq? 'just-walk (trip-segment-method (content walk-to-met))))))

 (define-test (est-fly-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell fly-to-met)
    (add-content fly-to-met (make-trip-segment-key 'start 'home 'end 'met))
    (p:fast-air-estimate fly-to-met)
    (run)
    (check (estimate? (trip-segment-time (content fly-to-met))))
    (check (eq? 'fly (trip-segment-method (content fly-to-met))))))

 (define-test (est-subway-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell subway-to-met)
    (add-content subway-to-met (make-trip-segment-key 'start 'home 'end 'met))
    (p:fast-subway-estimate subway-to-met)
    (run)
    ;; TODO Estimates of impossibility?
    (check (estimate? (trip-segment-time (content subway-to-met))))
    (check (eq? 'subway (trip-segment-method (content subway-to-met))))))

 (define-test (air-splitter)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (define-cell fly-to-met)
    (define-cell beginning)
    (define-cell middle)
    (define-cell end)
    (add-content go? 'go-deep)
    (add-content fly-to-met (make-trip-segment-key 'start 'home 'end 'met))
    ((splitter e:pick-airport) go? fly-to-met beginning middle end)
    (run)
    (check (equal? '(home logan laguardia)
		   (map trip-segment-start
			(map content (list beginning middle end)))))
    (check (equal? '(logan laguardia met)
		   (map trip-segment-end
			(map content (list beginning middle end)))))))

 (define-test (fly-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (define-cell fly-to-met)
    (add-content go? 'go-deep)
    (add-content fly-to-met (make-trip-segment-key 'start 'home 'end 'met))
    ((force plan-air) go? fly-to-met)
    (run)
    (check (with-units? (trip-segment-time (content fly-to-met))))
    (check (eq? 'fly (trip-segment-method (content fly-to-met))))))

 (define-test (train-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (define-cell train-to-met)
    (add-content go? 'go-deep)
    (add-content train-to-met (make-trip-segment-key 'start 'home 'end 'met))
    ((force plan-train) go? train-to-met)
    (run)
    (check (with-units? (trip-segment-time (content train-to-met))))
    (check (eq? 'take-the-train (trip-segment-method (content train-to-met))))))

 (define-test (subway-to-logan)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (define-cell subway-to-logan)
    (add-content go? 'go-deep)
    (add-content subway-to-logan (make-trip-segment-key 'start 'home 'end 'logan))
    ((force plan-subway) go? subway-to-logan)
    (run)
    (check (with-units? (trip-segment-time (content subway-to-logan))))
    (check (eq? 'subway (trip-segment-method (content subway-to-logan))))))

 (define-test (trip-to-met)
   (interaction
    (initialize-scheduler)
    (define-cell go?)
    (add-content go? 'go-deep)
    (define-cell trip-to-met)
    (add-content trip-to-met (make-trip-segment-key 'start 'home 'end 'met))
    ((force plan-trip) go? trip-to-met)
    (run)
    (check (with-units? (trip-segment-time (content trip-to-met))))
    (check (eq? 'take-the-train (trip-segment-method (content trip-to-met))))))
)
