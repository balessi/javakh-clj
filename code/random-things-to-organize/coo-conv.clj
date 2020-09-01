
; From http://www.stargazing.net/kepler/b1950.html

(defn signal [n] (if (zero? n) 1.0 (/ n (Math/abs n))))

(defn deg->rad [ang] (Math/toRadians ang))
(defn rad->deg [ang] (Math/toDegrees ang))

(defn sin-deg [ang] (Math/sin (deg->rad ang)) )
(defn cos-deg [ang] (Math/cos (deg->rad ang)) )

(defn sexa->deg [rah ram ras ded dem des]
    (let [
        ra   (* (+ rah (/ ram 60.0) (/ ras 3600.0)) 15.0)
        sig  (signal ded)
        ded2 (Math/abs ded)
        de   (* sig (+ ded2 (/ dem 60.0) (/ des 3600.0) ))
    ]
    [ra de])
)

(defn format-dec [n d] (. (BigDecimal. n) setScale d BigDecimal/ROUND_HALF_EVEN) )

(defn deg->sexa
    ([[ra de]] (deg->sexa ra de))
    ([ra de]
    (let [
        rah  (/ ra 15.0)
        rahi (int rah)
        ram  (* (- rah rahi) 60.0)
        rami (int ram)
        ras  (* (- ram rami) 60.0)
        sig  (signal de)
        ded  (Math/abs de)
        dedi (int ded)
        dem  (* (- ded dedi) 60.0)
        demi (int dem)
        des  (* (- dem demi) 60.0)
        ra2  (clojure.string/join " " [rahi rami (format-dec ras 1)])
        de2  (clojure.string/join " " [(int (* sig dedi)) demi (format-dec des 0)])
    ]
    (clojure.string/join "  " [ra2 de2])))
)

(defn xx [[ra de]] (* (cos-deg ra) (cos-deg de)))
(defn yy [[ra de]] (* (sin-deg ra) (cos-deg de)))
(defn zz [[_  de]] (sin-deg de))

;valores tirados de 1983A&A_128_263A (Aoki et al.)
(defn xp [x y z] (+ (* 0.9999256782 x) (* -0.0111820610 y) (* -0.0048579477 z) ) )
(defn yp [x y z] (+ (* 0.0111820609 x) (*  0.9999374084 y) (* -0.0000271765 z) ) )
(defn zp [x y z] (+ (* 0.0048579479 x) (* -0.0000271474 y) (*  0.9999881997 z) ) )

(defn B1950->J2000 [rah ram ras ded dem des]
    (let [
        coo  (sexa->deg rah ram ras ded dem des)
        x    (xx coo)
        y    (yy coo)
        z    (zz coo)
        x2   (xp x y z)
        y2   (yp x y z)
        z2   (zp x y z)
        at2  (Math/atan (/ y2 x2))
        ra2k (rad->deg(if (< x 0) (+ at2 Math/PI) (if (and (< y 0) (> x 0)) (+ at2 (* 2 Math/PI)) at2) ) )
        de2k (rad->deg (Math/asin z2))
    ]
    (deg->sexa ra2k de2k))
)

;(println "10 33 42.5 -9 20 54 --> " (deg->sexa (sexa->deg 10 33 42.5 -9 20 54)))

;pole st    B1950: 01 48 42 +89 02 00  J2000: 02 31 44.59 +89 16 07.7
;(prn (B1950->J2000 1 48 42 +89 2 0))

;eta per    B1950: 02470n5541  J2000: 02507n5553
;(prn (B1950->J2000 2 47 0 +55 41 0))

;alpha tau  B1950: 04330n1625  J2000: 04359n1631
;(prn (B1950->J2000 4 33 0 +16 25 0))

;alpha leo  B1950: 10057n1212  J2000: 10084n1157
;(prn (B1950->J2000 10 5 42 +12 12 0))

;delta sco  B1950: 15573s2229  J2000: 16003s2238
;(prn (B1950->J2000 15 57 18 -22 29 0))

;alpha eri  B1950: 01359s5730  J2000: 01377s5715
;(prn (B1950->J2000 1 35 54 -57 30 0))


;EQUATORIAL <-> GALACTIC

;from http://physics.stackexchange.com/questions/88663/converting-between-galactic-and-ecliptic-coordinates
;a=alpha, d=delta

;aG= 192.85, dG= +27.13
;aB= 266.40, dB= -28.94

;eq -> gal

;sin b = sin dG sin d + cos dG cos d cos(a-aG)
;cos b sin(122.9-l) = cos d sin(a-aG)
;cos b cos(122.9-l) = cos dG sin d - sin dG cos d cos(a-aG)

;b = asin(sin dG sin d + cos dG cos d cos(a-aG))
;l = 122.9 - asin((cos d sin(a-aG))/cos b)

;gal -> eq

;sin d = sin dG sin b + cos dG cos b cos(122.9-l)
;cos d sin(a-aG) = cos b sin(122.9-l)
;cos d cos(a-aG) = cos dG sin b - sin dG cos b cos(122.9-l)

;d = asin(sin dG sin b + cos dG cos b cos(122.9-l))
;a = asin((cos b sin(122.9-l))/cos d) + aG

(def aG 192.85)
(def dG 27.13)
(def pa 122.9)

(defn asin-deg [ang] (rad->deg (Math/asin (deg->rad ang))))

(defn gal->eq [l b]
    (let [
        d (asin-deg (+ (* (sin-deg dG) (sin-deg b)) (* (cos-deg dG) (cos-deg b) (cos-deg (- pa l)))) )
        a (+ (asin-deg (/ (* (cos-deg b) (sin-deg (- pa l))) (cos-deg d))) aG)
    ]
    (deg->sexa a d))
)

;N2573  25.405542  -89.334528  01h41m37.33s  -89d20m04.3s   302.7686 -27.7778
;N4889 195.0338750  27.9770000 13h00m08.130s +27d58m37.20s  57.1966  +87.8938

(println "TESTE")
(println (gal->eq 302.7686 -27.7778))
(println (gal->eq  57.1966 +87.8938))
