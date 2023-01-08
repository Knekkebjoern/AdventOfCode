(ns aoc.year2020.day1
  (:gen-class))
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map #(Integer/parseInt %) lines)]
    data))

(defn solve-fn [coll target cnt]
  (if (= 1 cnt)
    (if (.contains coll target)
      [target]
      [])
    (loop [c coll]
      (if (empty? c)
        []
        (let [res (solve-fn (rest c) (- target (first c)) (dec cnt))]
          (if (empty? res)
            (recur (rest c))
            (conj res (first c))))))))

(defn solve []
  (let [input (get-input "inputs/2020/day1.txt")
        part1 (solve-fn input 2020 2)
        part2 (solve-fn input 2020 3)]
    {:part1 (apply * part1)
     :part2 (apply * part2)
     }))

(defn status [] "*")
