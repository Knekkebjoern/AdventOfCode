(ns aoc.year2022.day13)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn get-input [filename]
  (loop [lines (str/split (slurp filename) #"\n") data []]
    (if (empty? lines)
      data
      (if (empty? (first lines))
        (recur (rest lines) data)
        (recur (rest lines) (conj data (eval (read-string (first lines)))))))))

(defmulti eval-pair
  (fn [x y] (mapv class [x y])))

(defmethod eval-pair [Number Number] [left right]
  (cond
    (= left right) nil
    (< left right) :correct
    :else :wrong))

(defmethod eval-pair [clojure.lang.PersistentVector clojure.lang.PersistentVector] [lefts rights]
  (loop [lefts lefts
         rights rights]
    (case [(empty? lefts) (empty? rights)]
      [true false] :correct
      [false true] :wrong
      [true true] nil
      [false false] (let [res (eval-pair (first lefts) (first rights))]
                      (if-not (nil? res)
                        res
                        (recur (rest lefts) (rest rights)))))))

(defmethod eval-pair [Number clojure.lang.PersistentVector] [left right]
  (eval-pair [left] right))

(defmethod eval-pair [clojure.lang.PersistentVector Number] [left right]
  (eval-pair left [right]))

(defn solve1 [pairs]
  (reduce + (map-indexed (fn [idx res] (if (= res :correct) (inc idx) 0))
                         (for [pair pairs]
                           (apply eval-pair pair)))))

(defn solve2 [input]
  (let [input (conj input [[2]] [[6]])
        sorted (sort (fn [x y]
                       (case (eval-pair (vec x) (vec y))
                         :correct -1
                         :wrong 1
                         0)) input)
        indexed (map-indexed (fn [idx res]
                               (case res
                                 [[2]] (inc idx)
                                 [[6]] (inc idx)
                                 1))
                             sorted)]
    (reduce * indexed)))

(defn solve []
  (let [input (get-input "inputs/2022/day13.txt")
        part1 (solve1 (partition 2 input))
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
