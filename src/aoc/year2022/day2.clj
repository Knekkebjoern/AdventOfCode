(ns aoc.year2022.day2)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map #(str/split % #"\s+") lines)
        data (for [x data] (map keyword x))]
    data))

(def shape-scores {:X 1 :Y 2 :Z 3})

(defn get-score [[a b]]
  (case [a b]
    ((:A :Z) (:B :X) (:C :Y)) (+ 0 (get shape-scores b)) ; lose
    ((:A :Y) (:B :Z) (:C :X)) (+ 6 (get shape-scores b)) ; win
    ((:A :X) (:B :Y) (:C :Z)) (+ 3 (get shape-scores b)) ; draw
    :unknown))

(defn solve1 [data]
  (reduce + (map get-score data)))

(def translate-shape
  {
   [:A :X] [:A :Z]
   [:A :Y] [:A :X]
   [:A :Z] [:A :Y]

   [:B :X] [:B :X]
   [:B :Y] [:B :Y]
   [:B :Z] [:B :Z]

   [:C :X] [:C :Y]
   [:C :Y] [:C :Z]
   [:C :Z] [:C :X]
   })

(defn solve2 [data]
  (reduce + (map get-score (for [x data] (get translate-shape x)))))

(defn solve []
  (let [input (get-input "inputs/2022/day2.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
