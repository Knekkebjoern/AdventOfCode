(ns aoc.year2020.day6
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

(defn solve []
  {:part1 (reduce + (map count (get-input1 "inputs/2020/day6.txt")))
   :part2 (reduce + (map count (map #(apply set/intersection %) (get-input2 "inputs/2020/day6.txt"))))})

(defn status [] "*")
