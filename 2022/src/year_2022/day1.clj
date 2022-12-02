(ns year_2022.day1)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (filter #(not= % '("")) (partition-by #(= % "") lines ))
        data (map #(map (fn [a] (Integer/parseInt a)) %) data)]
    data))

(defn solve1 [data]
  (apply max (map #(reduce + %) data)))

(defn solve2 [data]
  (apply + (take 3 (reverse (sort (map #(reduce + %) data))))))

(let [input (get-input "inputs/day1.txt")
      part1 (solve1 input)
      part2 (solve2 input)]
  (println "Part1: " part1)
  (println "Part2: " part2))
