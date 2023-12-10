(ns aoc.year2023.day9
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 9)
  )

(def input-filename "inputs/2023/day9.txt")


(defn get-input []
  (let [lines (for [line (str/split (slurp input-filename) #"\n")]
                (read-string (str "[" line "]")))]
    lines))

(defn find-next [series]
  (loop [series series acc []]
    (let [diffs (reverse (map #(apply - %) (partition 2 1 (reverse series))))]
      (if (every? #(= 0 %) diffs)
        (reduce + (flatten [(last series) acc]))
        (recur diffs (conj acc (last series)))))))

(defn solve1 [input]
  (reduce + (map find-next input)))

(defn solve2 [input]
  (reduce + (map find-next (map reverse input))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
