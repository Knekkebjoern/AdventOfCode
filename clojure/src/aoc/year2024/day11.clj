(ns aoc.year2024.day11
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 11))

(def input-filename "inputs/2024/day11.txt")

(defn get-input []
  (re-seq #"\d+" (io/read-file input-filename)))

(def count-stones (memoize (fn [stone steps]
                             (if (= steps 0)
                               1
                               (cond
                                 (= "0" stone) (count-stones "1" (dec steps))
                                 (even? (count stone)) (let [[a b] (->> stone
                                                                        (split-at (/ (count stone) 2))
                                                                        (map #(apply str %))
                                                                        (map parse-long)
                                                                        (map str))]
                                                         (+ (count-stones a (dec steps))
                                                            (count-stones b (dec steps))))
                                 :else (count-stones
                                        (doall (str (* 2024 (parse-long stone))))
                                        (dec steps)))))))

(defn solve1 [stones n]
  (reduce + (for [stone stones]
              (count-stones stone n))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input 25)
        part2 (solve1 input 75)]
    {:part1 part1 :part2 part2}))

(println (solve))

(defn status [] "*")
