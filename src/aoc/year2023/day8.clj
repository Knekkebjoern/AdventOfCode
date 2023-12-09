(ns aoc.year2023.day8
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [clojure.math :as math]))

(comment
  (aoc.core/fetch-input! 2023 8)
  )

;(def input-filename "inputs/2023/day8-test2.txt")
(def input-filename "inputs/2023/day8.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")
        instructions (first lines)
        nodes (apply merge (for [line (drop 2 lines)
                           :let [[id left right] (map first (re-seq #"([A-Z0-9]+)" line))]]
                       {id {:left left :right right}}))]
    {:instructions (into [] instructions) :nodes nodes}))

(defn solve1 [{:keys [instructions nodes]} start-nodes end-fn]
  (loop [instructions instructions
         current-nodes start-nodes
         steps 0]
    (if (end-fn current-nodes)
      steps
      (let [instruction (first instructions)
            action (if (= \L instruction) :left :right)
            next-nodes (into [] (for [node current-nodes]
                                  (get (get nodes node) action)))
            new-instructions (conj (vec (rest instructions))
                                   (first instructions))]
        (recur new-instructions next-nodes (inc steps))))))

(defn find-cycle [instructions nodes node]
  (loop [instructions instructions
         current-node node
         seen []
         steps 1]
    (if (= 2 (count seen))
      seen
      (let [;;a (println "seen" seen "current-node" current-node "steps" steps)
            instruction (first instructions)
            action (if (= \L instruction) :left :right)
            next-node (get (get nodes current-node) action)
            new-instructions (conj (vec (rest instructions))
                                   (first instructions))
            seen (if (= \Z (last next-node)) (conj seen steps) seen)]
        (recur new-instructions next-node seen (inc steps))))))

 (defn gcd ([x y] (cond (zero? x) y (< y x) (recur y x) :else (recur x (rem y x)))) ([x y & zs] (reduce gcd (gcd x y) zs)))

(defn lcm ([x y] (/ (* x y) (gcd x y))) ([x y & zs] (reduce lcm (lcm x y) zs)))

(defn solve2 [{:keys [instructions nodes]}]
  (let [start-nodes (filter #(= \A (last %)) (keys nodes))
        cycles (map #(find-cycle instructions nodes %) start-nodes)
        ;;cycles (find-cycle instructions nodes (first  start-nodes))
        diffs (map #(- (second %) (first %)) cycles)]
    (reduce lcm diffs)))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1  (solve1 input ["AAA"] (fn [current-nodes]
                                       (or (nil? current-nodes)
                                           (= ["ZZZ"] current-nodes))))
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
