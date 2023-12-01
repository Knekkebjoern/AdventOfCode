(ns aoc.year2023.day1
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [aoc.core]))

(comment
  (aoc.core/fetch-input! 2023 1)
  )

(def input-filename "inputs/2023/day1.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")]
    lines))

(defn solve1 [input]
  (reduce + (for [line input
                  :let [n (filter #(<= 48 (int %) 57) line)]] ;; \0 is 48, \9 is 57
              (Integer/parseInt (str (first n) (last n) )) )))

(def mappings {"zeroone" "01"
               "oneight" "18"
               "twone" "21"
               "threeight" "38"
               "nineight" "98"
               "eightwo" "82"
               "eighthree" "83"
               "zero" "0"
               "one" "1"
               "two" "2"
               "three" "3"
               "four" "4"
               "five" "5"
               "six" "6"
               "seven" "7"
               "eight" "8"
               "nine" "9"})

(def pattern (re-pattern "^(zerone|oneight|twone|threeight|nineight|eightwo|eighthree|zero|one|two|three|four|five|six|seven|eight|nine)"))

(defn str->num [s]
  (loop [s s res []]
    (if (empty? s)
      (apply str res)
      (let [[m _] (re-find pattern s)
            new-s (if (nil? m)
                    s
                    (str/replace-first s m (get mappings m)))]
        (recur (apply str (rest new-s)) (conj res (first new-s)) )
        )
      )))

(defn solve2 [input]
  (loop [input input res []]
    (if (empty? input)
      res
      (let [tmp (str->num (first input))]
        (recur (rest input) (conj res tmp))))))


(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve1 (solve2 input))]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
