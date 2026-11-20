(ns aoc.year2018.day1)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[clojure.set :as set])

(def input-filename "inputs/2023/dayX.txt")

(defn get-input []
  (let [lines (slurp input-filename)]
    ()))

(defn solve1 [input]
  (let []

    ))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        ;part2 (solve2 input max-x max-y)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
