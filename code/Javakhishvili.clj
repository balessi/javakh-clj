;---------------------------------------------------------------;
; 2006A&A...447..915J                                           ;
; Javakhishvili, G.; Kukhianidze, V.; Todua, M.; Inasaridze, R. ;
; A method of open cluster membership determination             ;
;---------------------------------------------------------------;
; Author: Bruno Alessi (bruno_alessi at yahoo dot com dot br)   ;
; Version: 28/set/2016 12:30                                    ;
;---------------------------------------------------------------;

(use '[clojure.string :only (split join)])
(import '(java.io File))


;*** GLOBALS **************************************************************************************

(def ^:dynamic *name* "./Alessi62.tsv")

(def ^:dynamic *max-rad* 15)
(def ^:dynamic *max-sa* 588)
(def ^:dynamic *max-se* 724)
(def ^:dynamic *num-bins* 6)


;*** MATH ****************************************************************************************

(defn max-value [coll] (reduce max coll))
(defn abs [n] (if (neg? n) (- n) n))
(defn abs-dif [x y] (abs (- x y)))
(defn sum-abs-dif [xi coll] (reduce + (map #(abs-dif xi %) coll)))
(defn between? [x l u] (and (>= x l) (< x u)))
(defn summ [coll] (reduce + coll))
(defn sq [x] (* x x))
(defn sq-dif [a b] (sq (- a b)))
(defn sum-dif [xi coll] (reduce + (map #(- xi %) coll)))
(defn exp [n] (Math/pow Math/E n))


;*** STATS ***************************************************************************************

(defn avg [coll] (/ (summ coll) (count coll)))
(defn std [coll]
  (let [av (avg coll) ff (partial sq-dif av)]
    (Math/sqrt (/ (summ (map ff coll)) (dec (count coll))))))

;Pn = e^(-2 * ((n-no)/sig_n)^2) ;P = Px * Py
(defn pn [n no sign] (exp (* -2 (sq (/ (- n no) sign)))))


;*** CONVERSIONS *********************************************************************************

(defn parse-dbl [^String s] (Double/parseDouble (. s trim)))
(defn parse-long [^String s] (Long/parseLong (. s trim)))
(defn to-long [^Double dbl] (. (Double/parseDouble (str "" dbl)) longValue))


;*** LISTS ***************************************************************************************

(defn third [coll] (second (rest coll)))
(defn fourth [coll] (third (rest coll)))
(defn fifth [coll] (fourth (rest coll)))
(defn sixth [coll] (fifth (rest coll)))
(defn seventh [coll] (sixth (rest coll)))


;*** STRINGS *************************************************************************************

(defn blank? [s] (every? #(Character/isWhitespace %) s))


;*** VizieR-Specific *****************************************************************************

(defn ignore-header [coll]
   (letfn [(ignore [s] (and (not (blank? s)) (= (. s substring 0 1) "#")))]
      (drop 3 (drop-while ignore (drop 1 (drop-while ignore coll))))))

(defn tsv-contents [name]
   (map #(split % #";") (ignore-header (split (slurp name) #"\n"))))


;*** FILES ***************************************************************************************

(defn file-path [fpathname]
   (let [last-sep (. fpathname lastIndexOf "/")]
      (. fpathname substring 0 (inc last-sep))))
         
(defn file-name [fpathname]
   (let [last-sep (. fpathname lastIndexOf "/")]
      (. fpathname substring (inc last-sep) (. fpathname lastIndexOf "."))))


;*** HISTOGRAM ***********************************************************************************

(defn bin-width [data] (Math/ceil (/ (max-value data) *num-bins*)))

(defn bins [data]
   (let [max-val (max-value data) bin-width (bin-width data)]
      (range 0 (+ max-val bin-width) bin-width)))

(defn- bin-filter [l u] (fn [x] (between? x l u)))

(defn bin-count [l u data] (count (filter (bin-filter l u) data)))

(defn bin-counts [data bins]
   (loop [bin bins counts []]
      (if (empty? bin)
         (reverse counts)
         (let [l (first bin) u (second bin)]
            (recur (rest bin) (cons (bin-count l u data) counts))))))

(defn counts [data] (bin-counts data (bins data)))

(defn histogram [data]
   (join "" (map
      #(str (format " %4d " (to-long %2)) (join "" (repeat %1 "X")) "\n")
      (counts data) (rest (bins data)))))


;*** JAVAKH-GAIA specific ************************************************************************

(defn hist-contents [name]
   (let [ignore (+ 8 (* 2 *num-bins*))]
      (map #(split % #";") (drop ignore (split (slurp name) #"\n")))))

(defn contents [name]
   (filter (fn [[_ rad]] (< rad *max-rad*))
      (map
         (fn [[rad tyc hip _ _ plx _ pmra _ pmde]]
            (let [desig (. (if (blank? tyc) (str "HIP " hip) (str "TYC " tyc)) trim)
                  rad-dbl (parse-dbl rad)
                  plx-dbl (parse-dbl plx)
                  pma-dbl (parse-dbl pmra)
                  pme-dbl (parse-dbl pmde)]
               [desig rad-dbl plx-dbl pma-dbl pme-dbl]))
          (tsv-contents name))))

(defn print-histograms [name]
   (let [path (file-path name)
         cluster (file-name name)
         cnt (contents name)
         des (map first cnt)
         rad (map second cnt)
         plx (map third cnt)
         xj (map fourth cnt)
         xs (map #(sum-abs-dif % xj) xj)
         yj (map fifth cnt)
         ys (map #(sum-abs-dif % yj) yj)
         mask "%-16s; %8.2f; %8.2f; %+10.3f; %6.0f; %+10.3f; %6.0f%n"
        ]
      (spit (File. (str path cluster ".hist"))
         (join "" [
            (str "\n" cluster " Histograms\n")
            (str "\n PmRA\n" (histogram xs))
            (str "\n PmDe\n" (histogram ys))
            "\n\n"
            (. ;gambi
               (join ""
                  (map #(format mask %1 %2 %3 %4 %5 %6 %7) des rad plx xj xs yj ys))
            replaceAll "," ".")
          ]))))

(print-histograms *name*)

(defn contents' [name]
   (filter (fn [[des r p sa pmra se pmde]] (and (< sa *max-sa*) (< se *max-se*)))
      (map
         (fn [[des rad plx pmra sa pmde se]]
            (let [sa-lng (parse-long sa)
                  r (parse-dbl rad)
                  p (parse-dbl plx)
                  pa-dbl (parse-dbl pmra)
                  se-lng (parse-long se)
                  pe-dbl (parse-dbl pmde)]
            [des r p sa-lng pa-dbl se-lng pe-dbl])
         )
         (hist-contents name))))

(defn pns [coll]
   (let [avg (avg coll) std (std coll)]
      (map #(pn % avg std) coll)))

(do
   (let [last-sep (inc (. *name* lastIndexOf "/"))
         path (. *name* substring 0 last-sep)
         cluster (. *name* substring last-sep (. *name* indexOf ".tsv"))
         name (str path cluster ".hist")
         des (map first (contents' name))
         rad (map second (contents' name))
         plx (map third (contents' name))
         xj (map fifth (contents' name))
         yj (map seventh (contents' name))
         mask "%-16s; %8.2f; %8.2f; %+10.3f; %+10.3f; %7.1f%n"
         fname (str path cluster ".memb")
         perc (map #(* %1 %2 100) (pns xj) (pns yj))
        ]
      (spit (File. fname)
         (str "\n" cluster "(num-bins, max-rad, max-sa, max-se) = (" *num-bins* ", " *max-rad* ", " *max-sa* ", " *max-se* ")\n\n"
            (join "" (map #(format mask %1 %2 %3 %4 %5 %6) des rad plx xj yj perc))))))
