
(ns tsv)

(use '[clojure.string :as st :exclude [reverse replace]])

(defn read-tsv [fname skip-first skip-last]
  (let [ lines (st/split-lines (slurp fname))
         lines (drop-last skip-last (drop skip-first lines))
         split-cols #(st/split % #";")
         matrix (map split-cols lines)
         matrix (map #(map st/trim %) matrix)
       ]
    matrix))

#_(read-tsv "C:/temp/test.tsv" 127 2)
