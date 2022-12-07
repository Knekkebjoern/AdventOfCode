(ns year_2022.day6)
(require '[clojure.set :as set])
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]    
    (first lines)))

(defn solve [line l]
  (loop [xs line n 0]
    (cond
      (empty? xs) n
      (= l (count (distinct (take l xs)))) (+ l n)
      :else (recur (rest xs) (inc n)))))

(let [input (get-input "inputs/day6.txt")
      part1 (solve input 4) 
      part2 (solve input 14)]
  (println "Part1: " part1)
  (println "Part2: " part2))
