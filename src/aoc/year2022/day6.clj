(ns aoc.year2022.day6)
(require '[clojure.set :as set])
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]
    (first lines)))

(defn solve1 [line l]
  (loop [xs line n 0]
    (cond
      (empty? xs) n
      (= l (count (distinct (take l xs)))) (+ l n)
      :else (recur (rest xs) (inc n)))))

(defn solve []
  (let [input (get-input "inputs/2022/day6.txt")
        part1 (solve1 input 4)
        part2 (solve1 input 14)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
