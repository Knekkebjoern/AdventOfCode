(ns aoc.year2024.day8
  (:require
   [aoc.io :as io]
   [clojure.math.combinatorics :as combo]))

(comment
  (aoc.core/fetch-input! 2024 8))

(def input-filename "inputs/2024/day8.txt")
;;(def input-filename "inputs/2024/day8-test.txt")

(defn get-input []
  (let [lines (io/parse-lines-as-grid (io/split-lines (io/read-file input-filename)))]
    lines))

;;              11
;;    012345678901
;; 0  ............
;; 1  ........0...
;; 2  .....0......
;; 3  .......0....
;; 4  ....0.......
;; 5  ......A.....
;; 6  ............
;; 7  ............
;; 8  ........A...
;; 9  .........A..
;; 10 ............
;; 11 ............

(defn get-antinodes [f locs]
  (let [pairs (filter (fn [[a b]] (not (= a b)))
                      (combo/selections locs 2))]
    (set (apply concat (for [pair pairs]
                         (apply f pair))))))

(defn get-antinode-1 [[x1 y1] [x2 y2]]
  (let [dx (abs (- x2 x1))
        dy (abs (- y2 y1))
        [ax bx] (cond
                  (> x1 x2) [(+ x1 dx) (- x2 dx)]
                  (< x1 x2) [(- x1 dx) (+ x2 dx)]
                  :else [x1 x2])
        [ay by] (cond
                  (> y1 y2) [(+ y1 dy) (- y2 dy)]
                  (< y1 y2) [(- y1 dy) (+ y2 dy)]
                  :else [y1 y2])]
    [[ax ay] [bx by]]))

(defn solve1 [input]
  (let [lookup (reduce (fn [res  [k v]]
                         (if (= \. v)
                           res
                           (update-in res [(keyword (str v))] conj k))) {} input)
        max-x (apply max (map first (keys input)))
        max-y (apply max (map second (keys input)))
        ids (keys lookup)
        res (into {} (for [id ids]
                       {id (get-antinodes get-antinode-1 (get lookup id))}))]
    (count (set (filter (fn [[x y]] (and (<= 0 x max-x)
                                         (<= 0 y max-y)))
                        (apply concat (vals res)))))))

(defn get-antinodes-in-area [[x y] [dx dy] [max-x max-y]]
  (loop [locs [[x y]]
         res #{}]
    (if (empty? locs)
      res
      (let [[x y] (first locs)]
        (cond
          (contains? res (first locs)) (recur (rest locs) res)
          (or (> x max-x)
              (> y max-y)
              (neg? x)
              (neg? y)) (recur (rest locs) res)
          :else (recur (conj (rest locs) [(+ x dx) (+ y dy)] [(- x dx) (- y dy)])
                       (conj res [x y])))))))

(defn get-antinodes-2 [locs [max-x max-y]]
  (let [pairs (filter (fn [[a b]] (not (= a b)))
                      (combo/selections locs 2))]
    (set (apply concat (for [[[ax ay] [bx by]]  pairs
                             :let [[dx dy] [(- bx ax) (- by ay)]]]
                         (get-antinodes-in-area [ax ay] [dx dy] [max-x max-y]))))))

(defn solve2 [input]
  (let [lookup (reduce (fn [res  [k v]]
                         (if (= \. v)
                           res
                           (update-in res [(keyword (str v))] conj k))) {} input)
        max-x (apply max (map first (keys input)))
        max-y (apply max (map second (keys input)))
        ids (keys lookup)
        res (into {} (for [id ids]
                       {id (get-antinodes-2 (get lookup id) [max-x max-y])}))]
    (count (set (filter (fn [[x y]] (and (<= 0 x max-x)
                                         (<= 0 y max-y)))
                        (apply concat (vals res)))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))
(defn status [] "*")
