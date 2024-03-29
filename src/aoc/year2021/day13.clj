(ns aoc.year2021.day13)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        points   (for [line (take-while #(not= % "") lines)
                       :let [[x y] (str/split line #",")]]
                   [(Integer/parseInt x) (Integer/parseInt y)])
        folds (for [[_ axis v] (filter some?
                                       (map #(re-find #"fold along ([x|y])=(\d+)" %) lines))]
                [axis (Integer/parseInt v)])]
    {:points points :folds folds}))

(defn show-points [points]
  (let [max_x (apply max (map first points))
        max_y (apply max (map second points))
        points-map (apply merge (for [p points] {p "#"}))
        d (for [y (range 0 (inc max_y))]
            (apply str (for [x (range 0 (inc max_x))]
                      (get points-map [x y] "."))))]
    (do
      (prn "")
      (dorun (for [l d] (prn l))))))

(defn fold-point [[axis v] [x y]]
  (case axis
        "x" (if (> v x)
              [x y]
              [(- v (- x v)) y])
        "y" (if (> v y)
              [x y]
              [x (- v (- y v))])
        ))

(defn solve1 [points folds]
  (if (empty? folds)
    points
    (let [new-points (map #(fold-point (first folds) %) points)]
      (recur new-points (rest folds)))))

(defn solve []
  (let [input (get-input"inputs/2021/day13.txt")
        part1     (count (into #{}
                               (solve1 (:points input) [(first (:folds input))])))
        part2 (show-points (solve1 (:points input) (:folds input)))]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
