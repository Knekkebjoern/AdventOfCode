(ns aoc.year2018.day2)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])

(def input-filename "inputs/2018/day2.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")]
    lines))

(defn solve1 [input]
  (loop [in input twos 0 threes 0]
    (if (empty? in)
      (* twos threes)
      (let [fr (frequencies (first in))
            cnts (set (vals fr))]
        (recur (rest in)
               (if (contains? cnts 2)
                 (inc twos)
                 twos)
               (if (contains? cnts 3)
                 (inc threes)
                 threes))))))

(defn cnt-diff [a b]
  (loop [a a b b cnt 0]
    (if (or (empty? a)
            (empty? b))
      cnt
      (recur (rest a) (rest b) (if (not= (first a) (first b))
                                 (inc cnt)
                                 cnt)))))

(defn solve2 [input]
  (for [a input
        b input
        :when (== 1 (cnt-diff a b))]
    (loop [a a b b res []]
      (if (empty? a)
        (apply str res)
        (let [new-res (if (= (first a) (first b))
                        (conj res (first a))
                        res)]
          (recur (rest a) (rest b) new-res))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
