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

(defn find-all [line]
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

(defn solve1 []
  (let [line (slurp input-filename)
        chars-per-line (str/index-of line "\n")
        padding  (apply str (concat (take chars-per-line (repeat "."))))
        line (apply str (concat "." padding "\n" line padding "\n"))
        line (str/replace line #"\n" "..")
        chars-per-line (+ chars-per-line 2)
        ps (find-all line)
        hits (filter #(touches-symbol? % line chars-per-line) ps)]
    (reduce + (map #(Integer/parseInt %) (map second hits)))
    ))

(defn solve2 [input]
  )

(defn solve []
  (let [[part1 part2] [nil nil]
        part1 (solve1)
        part2 (solve2)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
