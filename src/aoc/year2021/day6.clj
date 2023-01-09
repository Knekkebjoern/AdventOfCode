(ns aoc.year2021.day6)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data  (for [line lines]
                (str/split line #","))]
    (map #(Integer/parseInt %) (flatten data))))

(defn solve1 [gen data]
  (if (zero? gen)
    (reduce + (filter pos? (vals data)))
    (let [new-data (apply (partial merge-with +)
                           (for [k (keys data)
                                 :let [v (get data k 0)
                                       n (if (zero? k) {6 v 8 v}
                                             {(dec k) v})]]
                             n))]
      (recur (dec gen) new-data))))

(defn solve []
  (let [input (get-input "inputs/2021/day6.txt")
        part1 (solve1 80 (frequencies input))
        part2 (solve1 256 (frequencies input))]
    {:part1 part1 :part2 part2}))

;; part1 is wrong
(defn status [] "!")
