(ns year_2022.day4)
(require '[clojure.set :as set])
(require '[clojure.string :as str])


(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (partition 2 (for [pairs (map #(str/split % #",") lines)
                                [as bs] (map #(str/split % #"-") pairs)]
                            
                            (set (range (Integer/parseInt as) (inc (Integer/parseInt bs))))))]
    data ))

(defn solve1 [input]
  (count (filter (fn [[a b]] (or (set/subset? a b)
                               (set/subset? b a))) input)))

(defn solve2 [input]
  (count (filter (fn [[a b]] (not (empty? (set/intersection a b)))) input)))

(let [input (get-input "inputs/day4.txt")
      part1 (solve1 input)
      part2 (solve2 input)]
  (println "Part1: " part1)
  (println "Part2: " part2))
