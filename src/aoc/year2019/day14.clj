(ns aoc.year2019.day14
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


(defn status [] "!")
