(ns aoc.year2023.day3
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 3)
  )

(def input-filename "inputs/2023/day3.txt")
;;(def input-filename "inputs/2023/day3-test.txt")

(def nums #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn get-input []
  (let [line (slurp input-filename)
        chars-per-line (str/index-of line "\n")
        padding  (apply str (concat (take chars-per-line (repeat "."))))
        line (apply str (concat "." padding "\n" line padding "\n"))
        line (str/replace line #"\n" "..")
        chars-per-line (+ chars-per-line 2)]
    [line chars-per-line]))

(defn find-all-nums [line]
  (loop [line line i 0 res []]
    (if (empty? line)
      res
      (let [num (apply str (take-while #(contains? nums %) line))
            l (count num)]
        (recur (drop (+ 1 l) line) (+ 1 i l) (if (pos? l)
                                               (conj res [i num])
                                               res))))))

(defn touches-symbol? [[ind s] line chars-per-line]
  (let [left (dec ind)
        leftp (- left chars-per-line)
        leftn (+ left chars-per-line)
        right (+ ind (count s))
        rightp (- right chars-per-line)
        rightn (+ right chars-per-line)
        around (apply str (subs line leftp (inc rightp))
                      (subs line left (inc right))
                      (subs line leftn (inc rightn)))]
    (not (empty? (str/replace around #"\.|\d" "")))))

(defn solve1 [line chars-per-line]
  (let [ps (find-all-nums line)
        hits (filter #(touches-symbol? % line chars-per-line) ps)]
    (reduce + (map #(Integer/parseInt %) (map second hits)))
    ))


(defn find-all-gears [line]
  (loop [line line i 0 res []]
    (if (empty? line)
      res
      (let [num (apply str (take-while #(= \* %) line))
            l (count num)]
        (recur (drop (+ 1 l) line) (+ 1 i l) (if (pos? l)
                                               (conj res [i num])
                                               res))))))

(defn get-gear-ratio [pos indices chars-per-line]
  (let [next-to (fn [x] [(dec x) x (inc x)])
        is (concat (next-to pos)
                   (next-to (- pos chars-per-line))
                   (next-to (+ pos chars-per-line)))
        nums  (distinct (for [i is
                              :when (get indices i)]
                          (get indices i)))]
    (println nums)
    (if (=  (count nums) 2)
      (apply * nums)
      nil)))

(defn solve2 [line chars-per-line]
  (let [ps (find-all-nums line)
        indices (loop [ps ps res {}]
                  (if (empty? ps)
                    res
                    (let [[x s] (first ps)
                          xs  (apply merge (for [xx (range x  (+ x (count s)))]
                                             {xx (Integer/parseInt s)}))]
                      (recur (rest ps) (apply merge res xs)))))
        gears (find-all-gears line)
        ratios (for [[pos _] gears]
                 (get-gear-ratio pos indices chars-per-line))]
    (apply + (filter #(not (nil? %)) ratios))
    )
  )

(defn solve []
  (let [[part1 part2] [nil nil]
        [line chars-per-line] (get-input)
        part1 (solve1 line chars-per-line)
        part2 (solve2 line chars-per-line)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
