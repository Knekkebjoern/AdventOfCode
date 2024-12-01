(ns aoc.year2024.day1)

(comment
  (aoc.core/fetch-input! 2024 1))

(def input-filename "inputs/2024/day1.txt")

(defn get-input []
  (let [numbers (clojure.edn/read-string (str "["  (slurp input-filename) "]"))
        cols ((juxt #(sort (map first %)) #(sort (map second %))) (partition 2 numbers))]
    cols))

(defn solve1 [input]
  (let [pairs (partition 2 (apply interleave input))
        distances (map abs (map #(apply - %) pairs))]
    (reduce + distances)))

(defn solve2 [[col1 col2]]
  (let [f2 (frequencies col2)]
    (reduce + (for [x col1]
                (* x (get f2 x 0))))))

(defn solve []
  (let [input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))

(defn status [] "*")
