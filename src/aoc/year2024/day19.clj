(ns aoc.year2024.day19
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2024 19)
  )

(def input-filename "inputs/2024/day19.txt")

(defn get-input []
  (let [lines (slurp input-filename)]
    ))

(defn solve1 [input]
  )

(defn solve2 [input]
  )

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")