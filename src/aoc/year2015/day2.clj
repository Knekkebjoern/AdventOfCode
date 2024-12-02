(ns aoc.year2015.day2
  (:require  [aoc.io :as io]
             [clojure.string :as str]))

(comment
  (aoc.core/fetch-input! 2015 2))

(def input-filename "inputs/2015/day2.txt")

(defn get-input [filename]
  (let [lines (aoc.io/split-lines (aoc.io/read-file filename))
        dims (doall (for [l lines]
                      (clojure.edn/read-string (str "["  (str/replace l #"x" " ") "]"))))]

    dims))

(defn solve1 [data]
  (reduce (fn [total [l w h]]
            (let [[a b c] [(* w l) (* w h) (* h l)]]
              (+ total
                 (* 2 a) (* 2 b) (* 2 c)
                 (min a b c))))
          0 data))

(defn solve2 [data]
  (reduce (fn [total [l w h]]
            (let [[a b] (take 2 (sort [l w h]))]
              (+ total (* 2 a) (* 2 b) (* l w h))))
          0 data))

(defn solve []
  (let [input (get-input input-filename)
        part1 (solve1 input)
        part2 (solve2 input)]
    (println {:part1 part1 :part2 part2})))

(solve)
(defn status [] "*")
