(ns aoc.year2025.day1
  (:require
   [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2025 1))

(def input-filename "inputs/2025/day1.txt")

(defn get-input []
  (let [lines (io/split-lines (io/read-file input-filename))]
    (for [line lines]
      [(first line) (Integer/parseInt (apply str (rest line)))])))

(defn solve1 [input]
  (loop [current 50
         zeros 0
         turns input]
    (if (empty? turns)
      zeros
      (let [[direction clicks] (first turns)
            clicks (rem clicks 100)
            new-current   (if (= direction \L) (- current clicks) (+ current clicks))
            new-current (if (neg? new-current) (+ new-current 100) (rem new-current 100))
            new-current (if (= new-current 100) 0 new-current)]
        (recur  new-current (if (= new-current 0) (inc zeros) zeros) (rest turns))))))

(defn solve2 [input])

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
