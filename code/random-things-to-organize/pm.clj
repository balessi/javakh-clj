
(defn pm-angle [pmra pmde]
   (let [
      pra (Math/toRadians (/ pmra 3.6E6))
      pde (Math/toRadians (/ pmde 3.6E6))
      pm  (Math/sqrt (+ (* pmra pmra) (* pmde pmde)))
      ang (Math/toDegrees (Math/atan2 pde pra))
      ]
      [pm (rem (- 450 ang) 360)]
   )
)

(defn pm-comps [pm angle]
   (let [
      ang  (Math/toRadians angle)
      pmra (* pm (Math/sin ang))
      pmde (* pm (Math/cos ang))
      ]
      [pmra pmde]
))

(defn- check [name pmra pmde]
   (pr name [pmra pmde] " -> ")
   (let [pm-ang (pm-angle pmra pmde)]
      (prn pm-ang)
      ;the line below is only for re-check
      ;(prn (pm-comps (pm-ang 0) (pm-ang 1)))
   )
  ;(println "\n")
)

;(check "SCR J0821-6703    "  -386.9 656.8)  
