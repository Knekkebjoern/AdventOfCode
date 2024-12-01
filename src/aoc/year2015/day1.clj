(ns aoc.year2015.day1
  (:require
   [aoc.io]))

(comment
  (aoc.core/fetch-input! 2015 1))

(def input-filename "inputs/2015/day1.txt")

(defn get-input [filename]
  (aoc.io/read-file filename))

(defn solve1 [data]
  (reduce (fn [total x]
            (case x
              \( (inc total)
              \) (dec total))) 0 data))

(defn solve2 [data]
  (loop [data data index 0 floor 0]
    (if (or (neg? floor)
            (empty? data))
      index
      (let [new-floor (case (first data)
                        \( (inc floor)
                        \) (dec floor))]
        (recur (rest data) (inc index) new-floor)))))

(defn solve []
  (let [input (get-input input-filename)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
