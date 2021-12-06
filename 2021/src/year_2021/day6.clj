(ns year_2021.day6)
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

(let [input1 (get-input "inputs/day6_input1.txt")
      part1 (solve1 80 (frequencies input1))
      part2 (solve2 256 (frequencies input1))]
  (println "Part1: " part1)
  (println "Part2: " part2))

