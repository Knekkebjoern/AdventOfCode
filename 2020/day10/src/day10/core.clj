(ns day10.core
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (let [values (for [line (str/split (slurp filename) #"\n")]
                (read-string line))]
    (conj values 0 (+ 3 (apply max values)))))

(defn solve1 [input]
  (frequencies (for [pair (partition 2 1 (sort input))]
                (apply - pair))))

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

(defn -main
  [& args]
  (let [input (get-input "input.txt")
        part1 (solve1 input)
        part2 (time (solve2 input))]
    (println "Part1:" part1)
    (println "Part2:" part2)))
