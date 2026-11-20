(ns aoc.year2024.day10
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 10))

(def input-filename "inputs/2024/day10.txt")
;;(def input-filename "inputs/2024/day10-test.txt")

(defn get-input []
  (update-vals
   (io/parse-lines-as-grid (io/split-lines (io/read-file input-filename)))
   (fn [v] (parse-long (str v)))))

(defn get-surrounding [lookup [px py]]
  (into {} (for [[x y] [[1 0] [-1 0] [0 1] [0 -1]]
                 :let [nx (+ px x)
                       ny (+ py y)]
                 :when (and (not (= 0 x y))
                            (contains? lookup [nx ny]))]
             {[nx ny] (get lookup [nx ny])})))

(defn next-steps-for-pos [lookup pos]
  (let [height (get lookup pos)
        surr (get-surrounding lookup pos)]
    (for [[k v] surr
          :when (= v (inc height))]
      k)))

(defn get-all-trails [lookup]
  (loop [trails (map vector (keys (filter (fn [[_ v]] (= v 0)) lookup)))
         res []]
    (if (empty? trails)
      res
      (let [trail (first trails)
            last-pos (last trail)]
        (if (= 9 (get lookup last-pos))
          (recur (rest trails) (conj res trail))
          (let [next-steps (next-steps-for-pos lookup last-pos)
                new-trails (map (fn [step] (conj trail step)) next-steps)
                trails (concat (rest trails) new-trails)]
            (recur trails res)))))))

(defn solve1 [input]
  (let [trails (get-all-trails input)]
    (count (set (for [trail trails]
                  [(first trail) (last trail)])))))

(defn solve2 [input]
  (count  (get-all-trails input)))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))

(defn status [] "*")
