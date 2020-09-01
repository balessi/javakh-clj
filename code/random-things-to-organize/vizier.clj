(ns vizier)

(require '(clojure [string :as st]))
(require 'util)
(use 'util)

(defn conv [t s] 
   (let [type (Character/toUpperCase (first t))]
      (cond
         (= \F type) (str->dbl s)
         (= \I type) (str->int s)
         :else       (when-not (empty? s) s))))

(defn meta? [s] (.startsWith s "#Column"))

(defn- cut [s] (st/join (butlast (rest s))))

(defn- meta-map [[_ name type desc ucd]]
   {:name (keyword name)
    :type (cut type)
    :valid nil
    :desc desc
    :ucd (cut ucd)})

; #Column n_RV  (A3)  Remark on RV   [ucd=meta.note]

(defn read-tsv [file-name]
   (let [rows  (split-rows (into-string file-name))
         meta  (map #(meta-map (split-tabs %)) (drop-take meta? rows))
         rows  (drop 3 (drop-drop meta? rows))
         cols  (map split-cols rows)
         cols  (map #(map st/trim %) cols)
         keyss (map :name meta)
         cols  (map #(zipmap keyss %) cols)
         ]
      cols))
