(ns aoc.year2020.day5
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-input [filename]
  (str/split (slurp filename) #"\n"))

(defn parse [s]
  (let [b (clojure.string/replace s #"B|F|R|L" {"B" "1" "F" "0" "R" "1" "L" "0"})
        r (Integer/valueOf (subs b 0 7) 2)
        c (Integer/valueOf (subs b 7) 2)]
    {:row r
     :column c
     :id (+ (* r 8) c)}
    ))

(defn solve []
  (let [data (map parse (get-input "inputs/2020/day5.txt"))
        ids (set (map #(get % :id) data))]
    {:part1 (apply max ids)
     :part2 (first (filter #(and (.contains ids (inc %))
                                 (.contains ids (dec %)))
                           (sort (set/difference (set (range 1024)) ids))))}))

(defn status [] "*")
