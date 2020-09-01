
(defn ** [x y] (Math/pow x y))

(def mag (** 100 1/5))

(def mags [13.25 10.5 9.0 10.5 9.75])                 

(defn total-mag [mags]
   (let [ sorted-mags (reverse (sort mags))
          faintest (first sorted-mags)
          others (rest sorted-mags)
          differences (map #(- faintest %) others)
          brightnesses (map #(** mag %) differences)
          total-bright (reduce + 1 brightnesses)
        ]
        (- faintest (* 2.5 (Math/log10 total-bright)))
   )
)   

(println (str "the sum of " mags " is: " (total-mag mags)))

(total-mag (take 100 (repeat 10)))

(defn surface-brightness [mag diam]
   (+ mag (* 2.5 (Math/log10 (* diam diam)))))
