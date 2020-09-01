
(defn sin2 [a] (let [sin2a (Math/sin a)] (* sin2a sin2a)))

(defn havsin [a] (sin2 (/ a 2.0)))

(defn sign [n] (/ n (Math/abs n)))

(defn gcd [lam1 phi1 lam2 phi2]
  (let [phi1r (Math/toRadians phi1)
        lam1r (Math/toRadians lam1)
        phi2r (Math/toRadians phi2)
        lam2r (Math/toRadians lam2)
        term1 (havsin (Math/abs (- phi2r phi1r)))
        term2 (* (Math/cos phi1r) (Math/cos phi2r) (havsin (Math/abs (- lam2r lam1r))) )
       ]
       (Math/toDegrees (* 2 (Math/asin (Math/sqrt (+ term1 term2)))))))

(defn todeg [rah ram ras ded dem des]
  (let [radd (* (+ (/ ras 3600) (/ ram 60) rah) 15)
        ;_ (prn radd)
        dedd (* (sign ded) (+ (/ des 3600) (/ dem 60) (Math/abs ded)))
        ;_ (prn dedd)
       ]
       [radd dedd]))

(defn gcdd [rah1 ram1 ras1 ded1 dem1 des1 rah2 ram2 ras2 ded2 dem2 des2]
  (let [[radd1 dedd1] (todeg rah1 ram1 ras1 ded1 dem1 des1)
        ;_ (prn radd1 dedd1)
        [radd2 dedd2] (todeg rah2 ram2 ras2 ded2 dem2 des2)
        ;_ (prn radd2 dedd2)
        ]
        (gcd radd1 dedd1 radd2 dedd2)))
        
       
;21 08 46.83929 -88 57 23.3966 <--> 21 24 46.359 -88 46 30.57  sep = 711.89 

(* (gcdd 21 8 46.83929 -88 57 23.3966 21 24 46.359 -88 46 30.57) 3600)

(defn topc [mas] (/ 1000 mas))

(defn cosinelaw [x y a]
  (let [ x2 (* x x)
         y2 (* y y)
       ]
       (Math/sqrt (+ x2 y2 (* -2 x y (Math/cos (Math/toRadians a)))))))

(defn distance [rah1 ram1 ras1 ded1 dem1 des1 mas1 rah2 ram2 ras2 ded2 dem2 des2 mas2]
  (let [x (topc mas1)
        y (topc mas2)
        a (gcdd rah1 ram1 ras1 ded1 dem1 des1 rah2 ram2 ras2 ded2 dem2 des2)
       ]
       (cosinelaw x y a)))

(def pc->ly 3.26163)

;Gl 229 - Kapteyn = 9.3
;Gl 229 - Ross614 = 7.6
;06 10 34.6 -21 51 53    173.77
;05 11 40.6 -45 01 06    255.67
;06 29 23.4 -02 48 50    244.44

(* pc->ly (distance 6 10 34.6 -21 51 53 173.77 5 11 40.6 -45 1 6 255.67)) ;9.22
(* pc->ly (distance 6 10 34.6 -21 51 53 173.77 6 29 23.4 -2 48 50 244.44)) ;7.64

;Luhman 16 - Alp Cen = 3.63
;10 49 18.723 -53 19 09.86 495
;14 39 36.4951 –60 50 02.308 747.1

(* pc->ly (distance 10 49 18.723 -53 19 9.86 495 14 39 36.4951 –60 50 2.308 747.1)) ;3.63
(* pc->ly (distance 14 39 36.4951 –60 50 2.308 747.1 10 49 18.723 -53 19 9.86 495)) ;3.63
 
;Wolf 359 10 56 29.2 +07 00 53 419.10
;WISE 08 55 10.83 -07 14 42.5 433

(* pc->ly (distance 10 56 29.2 +7 0 53 419.1 8 55 10.83 -7 14 42.5 433)) ;4.41
(* pc->ly (distance 14 39 36.4951 –60 50 2.308 747.1 14 39 36.4951 –60 50 2.308 747.1)) ;0.0
(* pc->ly (distance 6 0 0.0 +10 0 0 1000 18 0 0.0 -10 0 0 1000)) ;6.52
(* pc->ly (distance 6 0 0.0 +10 0 0 1000 6 0 0.0 +10 0 0 500)) ;3.26
