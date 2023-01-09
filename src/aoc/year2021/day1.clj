(ns aoc.year2021.day1)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map #(Integer/parseInt %) lines)]
    data))

(defn solve1 [data]
  (count (for [[x y] (partition 2 1 data)
                  :when (< x y)]
              1)))

(defn solve2 [data]
  (solve1 (for [[x y z] (partition 3 1 data)]
            (+ x y z))))

(defn solve []
  (let [input (get-input "inputs/2021/day1.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
