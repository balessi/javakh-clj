
(import '(java.util Random))
(use '[clojure.string :only (join)])

(def *num-bins* 7)

(defn rnd-ger [] (. (Random.) nextInt 100))

(def data (repeatedly 25 rnd-ger))

(def max-value (reduce max data))

(def bin-width (Math/ceil (/ max-value *num-bins*)))

(def intervals (range 0 (+ max-value bin-width) bin-width))

(defn between?
   ([x l u] (and (>= x l) (< x u)))
   ([x u]   (between? x 0 u)))

(defn bin-filter [l u]
   (fn [x] (between? x l u)))

(defn bin-count [l u data]
   (count (filter (bin-filter l u) data)))

(defn bin-counts [data bins]
   (loop [bin bins counts []]
      (if (empty? bin)
         (reverse counts)
         (let [l (first bin) u (second bin)]
            (recur (rest bin) (cons (bin-count l u data) counts))))))

(def counts (bin-counts data intervals))

(defn to-int [^Double dbl] (. (Double/parseDouble (str "" dbl)) intValue))

(defn print-histogram [counts intervals]
   (dorun
      (map
         #(print (str (to-int %2) " " (join "" (repeat %1 "X")) "\n"))
       counts (rest intervals))))

(do
   (println)
   (println "..:.H.I.S.T.O.G.R.A.M.:..")
   (println "data .......: " data)
   (println "max-value ..: " max-value)
   (println "bin-width ..: " bin-width)
   (println "intervals ..: " intervals)
   (println "counts .....: " counts)
   (print-histogram counts intervals)
)
