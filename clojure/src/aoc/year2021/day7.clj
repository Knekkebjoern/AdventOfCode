(ns aoc.year2021.day7)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data  (for [line lines]
                (str/split line #","))]
    (map #(Integer/parseInt %) (flatten data))))

(defn fuel-to-point-1 [x data]
  (reduce + (map #(Math/abs %) (map #(- % x) data))))

(defn fuel-to-point-2 [x data]
  (let [dists (map #(Math/abs %) (map #(- % x) data))
        costs (for [d dists]
                (reduce + (range 1 (inc d))))]
    (reduce + costs)))

(defn solve1 [f data]
  (let [min-loc (apply min data)
        max-loc (apply max data)
        min-fuel (loop [least-fuel Integer/MAX_VALUE
                        locations (range min-loc (inc max-loc))]
                   (let [loc (first locations)
                         fuel (f loc data)]
                     (if (> fuel least-fuel)
                       {:loc (dec loc) :fuel least-fuel}
                       (recur fuel (rest locations)))))]
    min-fuel))

(defn solve []
  (let [input (get-input"inputs/2021/day7.txt")
        part1 (:fuel (solve1 fuel-to-point-1 input))
        part2 (:fuel (solve1 fuel-to-point-2 input))
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
