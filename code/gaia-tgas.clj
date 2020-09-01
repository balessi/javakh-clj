;(load-file "C:/Users/pc/Documents/paideia/Ast/_organize/gaia-tgas.clj")

(use '[clojure.string :only (split join)])
(import '(java.io File))

(defn blank? [s] (every? #(Character/isWhitespace %) s))

(defn file-contents [name] (map #(split % #";") (split (slurp name) #"\n")))

(def fin "C:/Users/pc/Documents/paideia/Ast/_organize/Gaia-DS1.dat")
(def fout "C:/Users/pc/Documents/paideia/Ast/_organize/Gaia-DS1-proc.dat")

(doall
(spit (File. fout)
   (join ""
   (map
      (fn [[desig ra de pmra epmra pmde epmde plx eplx mag]]
         (format
            "%-16s;%-14s;%-14s;%-6s;%-5s;%-9s;%-6s;%-9s;%-6s;%-6s%n"
            desig ra de pmra epmra pmde epmde plx eplx mag))
      (map
         (fn [[hip tyc ra de plx eplx pmra epmra pmde epmde mag]]
            (let [desig (if (blank? hip) (str "TYC " (. tyc trim)) (str "HIP "(. hip trim)))]
               [desig ra de pmra epmra pmde epmde plx eplx mag]))
         (file-contents fin))))))

; 13989;            ;045.0343303544;+00.2353916488;  6.35; 0.31;   43.752; 0.071;   -7.642; 0.087; 7.991
;TYC 1155-1018-1    ;045.1960001503;+00.9933051475;  6.58; 0.30;   16.401; 0.802;  -24.100; 0.539; 8.774
