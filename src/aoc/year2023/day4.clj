(ns aoc.year2023.day4
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [clojure.math :as math]))

(comment
  (aoc.core/fetch-input! 2023 4)
  )

(def input-filename "inputs/2023/day4.txt")
;;(def input-filename "inputs/2023/day4-test.txt")

(defn parse-line [line]
  (let [[id nums] (str/split line #":")
        id (Integer/parseInt (str/replace id #"Card\s+" ""))
        [winning have] (str/split nums #" \| ")
        winning (into #{}
                      (map #(Integer/parseInt %) (map first (re-seq #"(\d+)" winning))))
        have (into #{}
                   (map #(Integer/parseInt %) (map first (re-seq #"(\d+)" have))))]
    {:id id :winning winning :have have}))

(defn get-input []
  (map parse-line (str/split (slurp input-filename) #"\n")))

(defn solve1 [input]
  (loop [tickets input res 0]
    (if (empty? tickets)
      res
      (let [ticket (first tickets)
            matches (cset/intersection (:winning ticket) (:have ticket))
            points (case (count matches)
                     0 0
                     1 1
                     (math/pow 2 (dec (count matches))))]
        (recur (rest tickets) (+ res points)))))
  )

(defn solve2 [input]
  (apply +
         (vals (reduce (fn [res {:keys [id winning have]}]
                         (let [matched (count (cset/intersection winning have))
                               won (apply merge
                                          (for [nid (range (inc id) (+ id matched 1))]
                                            {nid (* (get res id 1) 1)}))]
                           (merge-with + res won)))
                       (frequencies (map :id input))
                       input))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
