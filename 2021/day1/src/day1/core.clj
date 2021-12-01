(ns day1.core)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map #(Integer/parseInt %) lines)]
    data))

(defn solve1 [data]
  (count (for [[x y] (partition 2 1 data)
                  :when (< x y)]
              1)))

(defn solve2 [data]
  (solve1 (for [[x y z] (partition 3 1 data)]
            (+ x y z))))

(let [input1 (get-input "input1.txt")
      input2 (get-input "input1.txt")
      part1 (solve1 input1)
      part2 (solve2 input2)]
  (println "Part1: " part1)
  (println "Part2: " part2))
