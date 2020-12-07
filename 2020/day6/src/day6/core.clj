(ns day6.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-input1 [filename]
  (for [entry (str/split (slurp filename) #"\r?\n\r?\n")]
    (set (str/replace entry #"\s+" ""))))

(defn get-input2 [filename]
  (for [entry (str/split (slurp filename) #"\r?\n\r?\n")]
    (for [answer (str/split entry #"\n")]
      (set answer))))

(defn -main
  [& args]
  (do
    (println "Part 1:" (reduce + (map count (get-input1 "input.txt"))))
    (println "Part 2:" (reduce + (map count (map #(apply set/intersection %) (get-input2 "input.txt")))))
    ))

(-main)
