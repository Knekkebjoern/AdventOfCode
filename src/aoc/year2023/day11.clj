(ns aoc.year2023.day11
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 11)
  )

;;(def input-filename "inputs/2023/day11-test.txt")
(def input-filename "inputs/2023/day11.txt")

(defn transpose [m]
  (apply mapv vector m))

(defn get-row-indices [rows expansion]
  (loop [rows rows i 0 ii 0res {}]
    (if (empty? rows)
      res
      (let [[ii res] (if (every? #(= \. %) (first rows))
                       [(+ ii expansion) (assoc res i (+ ii expansion))]
                       [(inc ii) (assoc res i ii)])]
        (recur (rest rows) (inc i) ii res)))))


(defn find-galaxies [x-indices y-indices rows]
  (loop [rows rows
         y 0
         res []]
    (if (empty? rows)
      res
      (let [row (first rows)
            galaxies (for [entry (map-indexed (fn [i p] [i p]) row)
                           :when (= \# (second entry))]
                       [(get x-indices (first entry))
                        (get y-indices y)])]
        (recur (rest rows) (inc y) (into res galaxies))))))

(defn get-input [expansion]
  (let [lines (str/split (slurp input-filename) #"\n")
        y-indices (get-row-indices lines expansion)
        x-indices (get-row-indices (transpose lines) expansion)
        data (find-galaxies x-indices y-indices lines)]
    data))

(defn get-distance [pair]
  (let [[x1 y1] (first pair)
        [x2 y2] (second pair)
        [dx dy] [(if (>= x1 x2) (- x1 x2) (- x2 x1))
                 (if (>= y1 y2) (- y1 y2) (- y2 y1))]]
    (+ dx dy)))

(defn solve1 [input]
  (let [pairs (into #{} (for [g1 input
                              g2 input
                              :when (not= g1 g2)]
                          #{g1 g2}))
        dists (for [pair pairs]
                (get-distance pair))]
    (reduce + dists)))

(defn solve []
  (let [[part1 part2] [nil nil]
        part1 (solve1 (get-input 1))
        part2 (solve1 (get-input 1000000))
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
