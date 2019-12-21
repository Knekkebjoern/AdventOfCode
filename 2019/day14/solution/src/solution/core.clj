(ns solution.core
  (:gen-class))
(require '[clojure.string :as str])


(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (for [line lines
                   :let [[pre post] (str/split line #" => ")
                         pres (str/split pre #",")
                         a (println "pres" pres)
                         a (println "post" post)]]
               pre)]
    data))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
