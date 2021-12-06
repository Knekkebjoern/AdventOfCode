(ns year_2021.day2)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]    
    (for [line lines
          :let [[d x] (str/split line #"\s+")]]
      [(keyword d) (Integer/parseInt x)])))

(defn solve1 [[x y] data]
  (if (empty? data)
    (* x y)
    (let [[d v] (first data)
          new-coord (case d
                      :forward [(+ x v) y]
                      :down    [x (+ y v)]
                      :up      [x (- y v)])]
      (recur new-coord (rest data)))))


(defn solve2 [[x y aim] data]
  (if (empty? data)
    (* x y)
    (let [[d v] (first data)
          new-coord (case d
                      :forward [(+ x v) (+ y (* aim v)) aim]
                      :down    [x y (+ aim v)]
                      :up      [x y (- aim v)])]
      (recur new-coord (rest data)))))


(let [input1 (get-input "inputs/day2_input1.txt")
      input2 (get-input "inputs/day2_input1.txt")
      part1 (solve1 [0 0] input1)
      part2 (solve2 [0 0 0] input2)]
  (println "Part1: " part1)
  (println "Part2: " part2))
