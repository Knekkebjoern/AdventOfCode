(ns day9.core
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

(defn solve [input n]
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

(defn -main
  [& args]
  (let [input (get-input "input.txt")
        part1 (solve input 25)
        part2 (solve2 input part1)]
    (println "Part1:" part1)
    (println "Part2:" part2)))
