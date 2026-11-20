(ns aoc.year2020.day9
  (:gen-class))

(require '[clojure.string :as str])

(defn get-input [filename]
  (concat
   (for [line (str/split (slurp filename) #"\n")]
     (BigInteger. line))))

(defn has-sum [xs x]
  (let [pairs (for [a xs b xs
                    :when (= x(+ a b))]
                [a b])]
    (not (empty? pairs))))

(defn solve1 [input n]
  (let [[preamble xs] (split-at 25 input)]
    (loop [preamble preamble xs xs]
      (if (empty? xs)
        nil
        (let [target (first xs)]
          (if (not (has-sum preamble target))
            target
            (recur (concat (rest preamble) [target]) (rest xs))))))))

(defn solve2 [input target]
  (loop [buf [] input input]
    (let [cur (reduce + buf)]
      (if (= target cur)
        (+ (apply min buf) (apply max buf))
        (if (> cur target)
          (recur (rest buf) input)
          (recur (concat buf [(first input)]) (rest input)))))))

(defn solve
  [& args]
  (let [input (get-input "inputs/2020/day9.txt")
        part1 (solve1 input 25)
        part2 (solve2 input part1)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
