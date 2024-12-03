(ns aoc.year2024.day3
  (:require
   [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 3))

(def input-filename "inputs/2024/day3.txt")

(defn get-input []
  (io/read-file input-filename))

(defn solve1 [input]
  (reduce + (for [line (io/split-lines input)
                  :let [matches (re-seq #"mul\((\d+),(\d+)\)" line)
                        mults (for [[_ a b] matches]
                                (* (Integer/parseInt a) (Integer/parseInt b)))]]
              (reduce + mults))))

(defn solve2 [input]
  (let [matches (re-seq #"(do\(\)|don\'t\(\)|mul\((\d+),(\d+)\))" input)]
    (loop [matches matches
           do true
           total 0]
      (if (empty? matches)
        total
        (let [[_ do-dont a b] (first matches)
              [do total] (case do-dont
                           "do()" [true total]
                           "don't()" [nil total]
                           [do (if do
                                 (+ total (* (Integer/parseInt a) (Integer/parseInt b)))
                                 total)])]
          (recur (rest matches) do total))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))

(defn status [] "*")
