(ns aoc.year2023.day2
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 2)
  )

(def input-filename "inputs/2023/day2.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")]
    (loop [lines lines games {}]
      (if (empty? lines)
        games
        (let [line (first lines)
              [whole id tmp] (re-matches #"^Game (\d+): (.*)$" line)
              turns (str/split tmp #"; ")
              balls (for [turn turns]
                      (apply merge (for [pull (str/split turn #", ")
                                   :let [[cnt color] (str/split pull #"\s+")]]
                               {color (Integer/parseInt cnt)})))]
          (recur (rest lines) (assoc games (Integer/parseInt id) balls)))))))

(defn valid-game? [game]
  (zero? (count
          (filter neg?
                  (vals (merge-with - {"red" 12 "green" 13 "blue" 14} game))))))

(defn valid-games? [games]
  (every? true? (map valid-game? games)))

(defn solve1 [input]
  (loop [games input cnt 0]
    (if (empty? games)
      cnt
      (let [[id turns] (first games)]
        (recur (rest games) (if (valid-games? turns)
                              (+ id cnt)
                              cnt))))))

(defn get-power [turns]
  (apply * (vals (reduce #(merge-with max %1 %2) turns))))

(defn solve2 [input]
  (loop [games input cnt 0]
    (if (empty? games)
      cnt
      (let [[id turns] (first games)
            power (get-power turns)]
        (recur (rest games) (+ power cnt))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
