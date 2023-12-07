(ns aoc.year2023.day5
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 5)
  )

;;(def input-filename "inputs/2023/day5-test.txt")
(def input-filename "inputs/2023/day5.txt")

(defn get-input []
  (let [lines (str/split (slurp input-filename) #"\n")
        seeds (map #(read-string %) (map first (re-seq #"(\d+)" (first lines))))
        maps (loop [lines (rest lines) cur nil res {}]
               (if (empty? lines)
                 res
                 (let [line (first lines)]
                   (if (empty? line)
                     (recur (next lines) cur res)
                     (let [[_ src dst] (re-matches #"(.*?)-to-(.*?) map:" line)]
                       (if (not (nil? src))
                         (recur (rest lines) [src dst] res)
                         (let [[dstid srcid cnt] (map #(read-string %)
                                                      (map first
                                                           (re-seq #"(\d+)" line)))

                               tmp [srcid (+ (dec cnt) srcid)
                                    dstid (+ (dec cnt) dstid)]

                               new-res (update-in  res cur conj tmp)]
                           (do
                             (recur (rest lines) cur new-res)))))))))]
    {:maps maps :seeds seeds}))

(defn find-in-range [id ranges]
  (if (empty? ranges)
    id
    (let [[ss se ds de] (first ranges)]
      (if (<= ss id se)
        (+ ds (- id ss))
        (find-in-range id (rest ranges))))))


(defn get-location [seed maps]
  (loop [src "seed" id seed]
    (if (= src "location")
      id
      (let [new-src (first (keys (get maps src)))
            new-id (find-in-range id (get (get maps src) new-src))]
        (recur new-src new-id)
        ))))

(defn solve1 [{:keys [maps seeds]}]
  (apply min (for [seed seeds]
               (get-location seed maps))))

(defn solve2 [input]
  (loop [ranges (partition 2 (:seeds input)) res []]
    (if (empty? ranges)
      (apply min res)
      (let [[start len] (first ranges)
            locations (for [seed (range start (+ start len 1))]
                        (get-location seed (:maps input)))
            cur-min (apply min locations)]
        (println "min" cur-min)
        (recur (rest ranges) (conj res cur-min))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
