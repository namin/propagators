(initialize-scheduler)

;; Problem 7.1: Warmup

;; a.

(define (make-person person)
  (let-cells (height weight)
    (eq-put! person 'height height)
    (eq-put! person 'weight weight)
    'done))

(make-person 'Nada)
(tell! (eq-get 'Nada 'height) (make-interval 168 172) 'nada-estimate)
(tell! (eq-get 'Nada 'weight) (make-interval 50 56) 'nada-estimate)

(make-person 'Fatso)
(tell! (eq-get 'Fatso 'height) 170 'nada-estimate)
(tell! (eq-get 'Fatso 'weight) 150 'nada-estimate)

(make-person 'Skinny)
(tell! (eq-get 'Skinny 'height) 170 'nada-estimate)
(tell! (eq-get 'Skinny 'weight) 45 'nada-estimate)

;; b.

(define (bmi person)
  (let ((height-m (ce:/ (eq-get person 'height) 100.0))
        (weight-kg (eq-get person 'weight)))
    (eq-put! person 'bmi (ce:/ weight-kg (ce:* height-m height-m)))
    ((c:bins (named-ranges 'bmi-estimate
                           `(underweight ,(make-interval 0.0 19.0))
                           `(normal      ,(make-interval 19.0 25.0))
                           `(overweight  ,(make-interval 25.0 30.0))
                           `(obese       ,(make-interval 30.0 100.0))))
     (eq-get person 'bmi))
    'done))

(bmi 'Nada)
(bmi 'Fatso)
(bmi 'Skinny)

(run)

(inquire (eq-get 'Nada 'bmi)) ;; 16.9 - 19.8
(tell! (eq-get (eq-get 'Nada 'bmi) 'normal) #t 'badria-estimate)
(inquire (eq-get 'Nada 'bmi)) ;; 19. - 19.8
(inquire (eq-get 'Nada 'weight)) ;; 54 - 56

(inquire (eq-get (eq-get 'Fatso 'bmi) 'obese)) ;; #t
(inquire (eq-get (eq-get 'Skinny 'bmi) 'underweight)) ;; #t