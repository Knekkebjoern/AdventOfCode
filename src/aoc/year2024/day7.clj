(ns aoc.year2024.day7
  (:require
   [aoc.io :as io]
   [clojure.math.combinatorics :as combo]))

(comment
  (aoc.core/fetch-input! 2024 7))

(def input-filename "inputs/2024/day7.txt")
;;(def input-filename "inputs/2024/day7-test.txt")

(defn get-input []
  (let [lines (io/split-lines (io/read-file input-filename))]
    (for [l lines]
      (map parse-long (re-seq #"\d+" l)))))

(defn my-eval [input-elems max-val]
  ;;(println "my-eval input-elems" input-elems "max-val" max-val)
  (loop [elems (rest input-elems)
         acc (first input-elems)]
    (cond
      (> acc max-val) nil
      (empty? elems) acc
      :else (let [[oper n] (take 2 elems)]
              (recur (drop 2 elems) (oper acc n))))))

(defn get-valid-expressions [opers args result]
  (let [oper-count (dec (count args))
        permutations (combo/selections opers oper-count)]
    (loop [permutations permutations
           res []]
      (if (empty? permutations)
        res
        (let [;; toss a nil at the end so that interleave doesn't drop the last arg
              exp (drop-last (interleave args (conj (vec (first permutations)) :tmp)))
              res (if (= result (my-eval exp result))
                    (conj res exp)
                    res)]
          (recur (rest permutations) res))))))

(defn solve1 [input]
  (reduce + (doall (for [[result & args] input
                         :let [exps (get-valid-expressions [+ *] args result)]]
                     (if (pos? (count exps))
                       result
                       0)))))

(defn || [a b]
  (parse-long (str (str a) (str b))))

(defn solve2 [input]
  (reduce + (doall (for [[result & args] input
                         :let [exps (get-valid-expressions [+ * ||] args result)]]
                     (if (pos? (count exps))
                       result
                       0)))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))
(defn status [] "*")
