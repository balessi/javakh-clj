
(defn sq [x] (* x x))

(defn sq-dif [x1 x2] (sq (- x2 x1)))

(defn dist
  ([x1 y1 x2 y2] (Math/sqrt (+ (sq-dif x1 x2) (sq-dif y1 y2))))
  ([x1 y1 z1 x2 y2 z2] (Math/sqrt (+ (sq-dif x1 x2) (sq-dif y1 y2) (sq-dif z1 z2)))))

(defn dist-to [x y z] (partial dist x y z))

(def points [[0 0 1][0 0 -1][1 0 0][-1 0 0][0 1 0][0 -1 0][0 0 0]])

(defn sum-dists [points]
  (let [dist-fns (map (fn [[x1 y1 z1]] (dist-to x1 y1 z1)) points)]
    ;dist-fns
    (reduce +
    (loop [dist-fn (first dist-fns) dist-fns' (rest dist-fns) dists []]
       (if (empty dist-fns')
         dists
         (recur 
     (empty
))
   


;(distances points)

