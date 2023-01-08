(ns aoc.year2020.day10
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (let [values (for [line (str/split (slurp filename) #"\n")]
                (read-string line))]
    (conj values 0 (+ 3 (apply max values)))))

(defn solve1 [input]
  (reduce * (vals (frequencies (for [pair (partition 2 1 (sort input))]
                                 (apply - pair))))))

(defn solve2 [input]
  (loop [input (reverse (sort input)) cache {}]
    (if (empty? input)
      (get cache (apply min (keys cache)))
      (let [target (first input)
            total (get cache target 1)
            compatible (take-while #(<= (- target %) 3) (rest input))
            updates (for [x compatible
                          :let [v (get cache x 0)]]
                      {x (+ total v)})
            newcache (apply merge cache updates)]
        (recur (rest input) newcache)))))

(defn solve
  [& args]
  (let [input (get-input "inputs/2020/day10.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
