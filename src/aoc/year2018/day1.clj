(ns aoc.year2018.day1)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[clojure.set :as set])

(def input-filename "inputs/2018/day1.txt")

(defn get-input []
  (let [lines (map #(Integer/parseInt %)
                   (str/split (slurp input-filename) #"\n"))]
    lines)
  )

(defn solve1 [input]
  (reduce + input))

(defn solve2 [input]
  (loop [in (cycle input) current 0 seen #{}]
    (let [n (first in)
          freq (+ current n)]
      ;;(println "current" current"+" (first in) " => " freq)
      (if (contains? seen freq)
        freq
        (recur (rest in) freq (conj seen freq))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
