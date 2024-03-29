(ns aoc.year2021.day5)
(require '[clojure.string :as str])

(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        coords (for [line lines
                     :let [[s e] (map #(str/split % #",")
                                      (str/split line #" -> "))]]
                 [(map #(Integer/parseInt %) s) (map #(Integer/parseInt %) e)])]
    coords))

(defn get-points [[[x1 y1] [x2 y2]]]
  (let [stepf (fn [a1 a2]
                (cond (< a1 a2) 1
                      (> a1 a2) -1
                      :else 0))
        xstep (stepf x1 x2)
        ystep (stepf y1 y2)]
    (loop [[x y] [x1 y1] res []]
      (if (= [x y] [x2 y2])
        (conj res [x y])
        (recur [(+ x xstep) (+ y ystep)] (conj res [x y]))))))

(defn solve1 [f data]
  (let [lines (if (nil? f)
                data
                (filter f data))
        points (apply concat (map get-points lines))
        freq (frequencies points)
        res (for [[k v] freq :when (> v 1)] {k v})]
    (count res)))

(defn solve []
  (let [input (get-input "inputs/2021/day5.txt")
        part1 (solve1 (fn [[[x1 y1] [x2 y2]]]
                        (or (= x1 x2) (= y1 y2))) input)
        part2 (solve1 nil input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
