(ns year_2022.day3)
(require '[clojure.set :as set])
(use '[clojure.string :only [index-of]])


(defn get-input [filename]
  (str/split (slurp filename) #"\n"))

(def priorities (apply str (map char (concat (range 97 123) (range 65 91)))))

(defn solve1 [input]
  (let [data (map (fn [s] (partition (/ (count s) 2) s)) input)]
    (reduce + (for [[as bs] data
                    :let [common (set/intersection (set as) (set bs))]]
                (inc (index-of priorities (first common)))))))


(defn solve2 [input]
  (let [data (partition 3 input)]
    (reduce + (for [group data
                    :let [common (apply set/intersection (map set group))]]
                (inc (index-of priorities (first common)))))))

(let [input (get-input "inputs/day3.txt")
      part1 (solve1 input)
      part2 (solve2 input)]
  (println "Part1: " part1)
  (println "Part2: " part2))
