(ns aoc.year2023.day6
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 6)
  )

(def input-filename "inputs/2023/day6.txt")
;;(def input-filename "inputs/2023/day6-test.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")
        times (map #(Integer/parseInt %) (map first (re-seq #"(\d+)" (first lines))))
        distances (map #(Integer/parseInt %) (map first (re-seq #"(\d+)" (second lines))))]
    (partition 2 (interleave times distances))))

(defn get-input2 []
  (let [[times distances] (map #(read-string %)
                               (map first (re-seq #"(\d+)"
                                                  (str/replace (slurp input-filename) #"\s+" ""))))]
    [[times distances]]))

(defn solve1 [input]
  (loop [input input res []]
    (if (empty? input)
      (reduce * res)
      (let [[t d] (first input)
            wins (for [hold (range 0 (inc t))
                       :let [moved (* hold (- t hold))]
                       :when (< d moved)]
                   moved)]
        (recur (rest input) (conj res (count wins)))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        part1 (solve1 (get-input))
        part2 (solve1 (get-input2))
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
