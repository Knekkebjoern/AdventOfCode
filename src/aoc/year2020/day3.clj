(ns aoc.year2020.day3
  (:gen-class))

(require '[clojure.string :as str])

(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (transpose (str/split (slurp filename) #"\n")))

(defn solve-fn [data step-right step-down]
  (let [end (count (first data))]
    (loop [data (cycle data) pos 0 cnt 0]
      (if (>= pos end)
        cnt
        (recur (drop step-right data)
               (+ pos step-down)
               (if (= \# (nth (first data) pos))
                 (inc cnt)
                 cnt))))))

(defn solve []
  (let [data (get-input "inputs/2020/day3.txt")]
    {:part1 (solve-fn data 3 1)
     :part2 (reduce * (for [[r d] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
                        (solve-fn data r d)))}))

(defn status [] "*")
