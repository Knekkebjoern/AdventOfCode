(ns aoc.year2024.day2
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 2))

(def input-filename "inputs/2024/day2.txt")

(defn get-input []
  (let [lines (io/split-lines (io/read-file input-filename))]
    (doall (for [l lines]
             (clojure.edn/read-string (str "[" l "]"))))))

(defn is-safe? [levels]
  (let [diffs (map (fn [[a b]]  (- b a))  (partition 2 1 levels))]
    (and (or (every? pos? diffs)
             (every? neg? diffs))
         (every? #(<= 1 (abs %) 3) diffs))))

(defn combinations [coll]
  (into [coll] (doall (for [i (range 0 (count coll))]
                        (into (vec (take i coll)) (drop (inc i) coll))))))

(defn is-safe-2? [levels]
  (some true? (map is-safe? (combinations levels))))

(defn solve1 [input]
  (count (filter is-safe? input)))

(defn solve2 [input]
  (count (filter true? (map is-safe-2? input))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    (println {:part1 part1 :part2 part2})))

(defn status [] "*")
