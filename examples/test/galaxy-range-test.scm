(in-test-group
 galaxy-range

 (define-test ()
   (interaction
    (define (+- value delta #!optional delta-)
      (let ((delta+ delta)
	    (delta- (if (default-object? delta-) delta delta-)))
	(make-interval (- value delta-) (+ value delta+))))

    (define (depends-on value . premises)
      (make-tms (contingent value premises)))

    (define (what-is cell)
      (let ((q (tms-query (content cell))))
	(cond ((vector? q)
	       (cond ((interval? (vector-ref q 1))
		      (if *center-difference-mode*
			  (let* ((value
				  (average (interval-low (vector-ref q 1))
					   (interval-high (vector-ref q 1))))
				 (delta
				  (- (interval-high (vector-ref q 1)) value)))
			    `(,(vector-ref q 0)
			      (+- ,value ,delta)
			      ,@(vector-ref q 2)))
			  `(,(vector-ref q 0)
			    (interval ,(interval-low (vector-ref q 1))
				      ,(interval-high (vector-ref q 1)))
			    ,@(vector-ref q 2))))
		     (else q)))
	      (else q))))

    (define *center-difference-mode* #f)
    
    (initialize-scheduler)


    ;;; Distance-modulus method

    (define-cell l10 (log 10))

    (define-propagator (p:mu->d mu d)
      (p:/ (e:exp (e:* (e:+ (e:/ mu 5) 1) l10))
	   1e6
	   d))

    (define-propagator (p:d->mu d mu)
      (p:* 5
	   (e:- (e:/ (e:log (e:* d 1e6)) l10) 1)
	   mu))

    (define-propagator (c:mu<->d mu d)
      (p:mu->d mu d)
      (p:d->mu d mu))

    ;;; M87 = NGC4486 -- The giant elliptical in Virgo Cluster

    (define-cell M87:distance-modulus)

    (define-cell M87:distance)

    (c:mu<->d M87:distance-modulus M87:distance)

    (add-content M87:distance-modulus
		 (depends-on (+- 31.43 0.3) 'VanDenBergh1985))

    (what-is M87:distance-modulus)
    (produces '(supported (interval 31.13 31.73) VanDenBergh1985))

    (run)

    (content M87:distance)

    (produces
     #(tms
       (#(supported
	  #(interval 16.826740610704718 22.181964198002227)
	  (VanDenBergh1985)))))

    (what-is M87:distance)
    (produces '(supported (interval 16.827 22.182) VanDenBergh1985))

    (explain M87:distance 1)
    (produces
     '(((m87:distance)
	has-value #(interval 16.827 22.182)
	by ((c:mu<->d) (m87:distance) (m87:distance-modulus))
	with-premises vandenbergh1985)
       ((m87:distance-modulus)
	has-value #(interval 31.13 31.73)
	by (user)
	with-premises vandenbergh1985)))

    (explain m87:distance 2)
    (produces
     '(((m87:distance)
	has-value #(interval 16.827 22.182)
	by ((p:mu->d) (m87:distance-modulus))
	with-premises vandenbergh1985)
     ((m87:distance-modulus)
      has-value #(interval 31.13 31.73)
      by (user)
      with-premises vandenbergh1985)))

    (explain m87:distance 3)
    (produces
     '(((m87:distance)
	has-value #(interval 16.827 22.182)
	by ((p:/) (1000000.) (cell1813))
	with-premises vandenbergh1985)
       ((cell1813) 
	has-value #(interval 16827000.0 22182000.0)
	by ((exp:p) (cell1810))
	with-premises vandenbergh1985)
       ((cell1810)
	has-value #(interval 16.638 16.915)
	by ((p:*) (2.3026) (cell1809))
	with-premises vandenbergh1985)
       ((cell1809)
	has-value #(interval 7.226 7.346)
	by ((+:p) (cell1804) (1))
	with-premises vandenbergh1985)
       ((cell1804)
	has-value #(interval 6.226 6.346)
	by ((p:/) (5) (m87:distance-modulus))
	with-premises vandenbergh1985)
       ((m87:distance-modulus)
	has-value #(interval 31.13 31.73)
	by (user)
	with-premises vandenbergh1985)))


    ;;; Surface-Brightness Fluctuation survey

    (add-content M87:distance
     (depends-on (+- 17 0.31) 'Tonry:SBF-IV))

    (run)

    (content M87:distance)
    (produces
     #(tms
       (#(supported
	  #(interval 16.826740610704718 17.30999999999999)
	  (VanDenBergh1985 Tonry:SBF-IV))
	#(supported
	  #(interval 16.69 17.31)
	  (Tonry:SBF-IV))
	#(supported
	  #(interval 16.826740610704718 22.181964198002227)
	  (VanDenBergh1985)))))
    ;;; The two measurements give a better intersection

    (what-is M87:distance)
    (produces '(supported (interval 16.827 17.31)
			  VanDenBergh1985
			  Tonry:SBF-IV))

    ;;; But note that the original measure is improved
    (content M87:distance-modulus)
   
    (produces
     #(tms
       (#(supported
	  #(interval 31.13 31.191485339376964)
	  (VanDenBergh1985 Tonry:SBF-IV))
	#(supported
	  #(interval 31.13 31.729999999999997)
	  (VanDenBergh1985)))))
   
    (what-is M87:distance-modulus)
    (produces '(supported (interval 31.13 31.191)
			  VanDenBergh1985
			  Tonry:SBF-IV))

    ;;; By redshift and Hubble law

    (define-cell :c 2.99792458e5)	;km/sec

    (define-propagator (p:z->v/c z v/c)
      (let-cells ((s (e:square (e:+ 1 z))))
	(p:/ (e:- s 1) (e:+ 1 s) v/c)))

    (define-propagator (p:v/c->z v/c z)
      (p:- (e:sqrt (e:/ (e:+ 1 v/c) (e:- 1 v/c)))
	   1
	   z))

    (define-propagator (c:v/c<->z v/c z)
      (p:v/c->z v/c z)
      (p:z->v/c z v/c))

    (define-propagator (c:v<->z v z)
      (let-cell v/c
	(c:* v/c :c v)
	(c:v/c<->z v/c z)))


    (define-cell M87:redshift
      (depends-on (+- 0.004360 0.000022) 'Smith2000))

    (what-is M87:redshift)
    (produces '(supported (interval .004338 .004382) Smith2000))

    (define-cell M87:radial-velocity)

    (c:v<->z M87:radial-velocity M87:redshift)


    (define-cell H0)			;Hubble constant

    (define-propagator (c:HubbleLaw d v)
      (c:* H0 d v))

    ;;; Early estimates of the Hubble constant were quite wild.
    (add-content H0
      (depends-on (+- 70 20) 'deVaucouleurs 'Sandage))

    (what-is H0)
    (produces '(supported (interval 50 90) deVaucouleurs Sandage))

    (c:HubbleLaw M87:distance M87:radial-velocity)

    (run)

    (what-is H0)
    (produces '(supported (interval 74.964 77.904)
			  Tonry:SBF-IV
			  VanDenBergh1985
			  Smith2000))
   
    (what-is M87:radial-velocity)
    (produces '(supported (interval 1297.6 1310.9) Smith2000))

    (add-content H0
     (depends-on (+- 73.5 3.2)		;km/sec/Mpc
		 'WMAP3))

    (run)
   
    (what-is M87:distance)
    (produces '(supported (interval 16.918 17.31)
			  WMAP3
			  Tonry:SBF-IV
			  VanDenBergh1985
			  Smith2000))


    (add-content H0
     (depends-on (+- 70.8 4)		;km/sec/Mpc
		 'WMAP:lCDM))

    (run)
    (produces
     '(contradiction
       (WMAP3
	Tonry:SBF-IV
	VanDenBergh1985
	Smith2000
	WMAP:lCDM)))
   

    (kick-out! 'Tonry:SBF-IV)

    (run)
   
    (what-is M87:distance)
    (produces '(supported (interval 17.348 18.647)
			  WMAP:lCDM
			  WMAP3
			  Smith2000))

    (what-is H0)
    (produces '(supported (interval 70.3 74.8)
			  WMAP3
			  WMAP:lCDM))

    (bring-in! 'Tonry:SBF-IV)
    (kick-out! 'WMAP:lCDM)

    (run)

    (what-is M87:distance)
    (produces '(supported (interval 16.918 17.31)
			  WMAP3
			  Tonry:SBF-IV
			  VanDenBergh1985
			  Smith2000))
   
    (what-is H0)
    (produces '(supported (interval 74.964 76.7)
			  Smith2000
			  VanDenBergh1985
			  Tonry:SBF-IV
			  WMAP3))

    ;; Consider another galaxy, NGC4889, the giant elliptical in the Coma
    ;; cluster, is well studied.

    ;; Here is the propagator network
    (define-cell NGC4889:distance)

    (define-cell NGC4889:distance-modulus)

    (define-cell NGC4889:redshift)

    (define-cell NGC4889:radial-velocity)

    (c:mu<->d NGC4889:distance-modulus NGC4889:distance)

    (c:v<->z NGC4889:radial-velocity NGC4889:redshift)

    (c:HubbleLaw NGC4889:distance NGC4889:radial-velocity)

    ;; Now for some data:


    (add-content NGC4889:distance-modulus
		 (depends-on (+- 34.875 0.015) 'Willick1997))

    (run)

    (add-content NGC4889:redshift
		 (depends-on (+- 0.021665 0.000043) 'Moore2002))

    (run)

    (produces
     '(contradiction (WMAP3 Tonry:SBF-IV VanDenBergh1985 Smith2000 Willick1997 Moore2002)))

    (kick-out! 'Tonry:SBF-IV)
    (run)

    (produces
     '(contradiction (Moore2002 Willick1997 WMAP3)))


    (bring-in! 'Tonry:SBF-IV)
    (kick-out! 'WMAP3)
    (run)
    (produces
     '(contradiction (Moore2002 Willick1997 Tonry:SBF-IV VanDenBergh1985 Smith2000)))


    (bring-in! 'WMAP3)
    (kick-out! 'Moore2002)
    (run)

    (what-is NGC4889:redshift)
    (produces
     '(supported (interval .023725 .024624)
		 Willick1997 Smith2000 VanDenBergh1985 Tonry:SBF-IV WMAP3))

    (what-is NGC4889:distance)
    (produces '(supported (interval 93.756 95.06) Willick1997))

    (what-is H0)
    (produces
     '(supported (interval 74.964 76.7) Smith2000 VanDenBergh1985 Tonry:SBF-IV WMAP3))

    (bring-in! 'Moore2002)
    (kick-out! 'Willick1997)
    (run)

    (what-is NGC4889:distance)
    (produces
     '(supported (interval 83.592 85.879) WMAP3 Tonry:SBF-IV VanDenBergh1985 Smith2000 Moore2002))

    (what-is H0)
    (produces
     '(supported (interval 74.964 76.7) Smith2000 VanDenBergh1985 Tonry:SBF-IV WMAP3))


    )

))
