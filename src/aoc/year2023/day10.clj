(ns aoc.year2023.day10
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 10)
  )

;;(def input-filename "inputs/2023/day10-test.txt")
(def input-filename "inputs/2023/day10.txt")

(defn get-input []
  (loop [chars (slurp input-filename)
         x 0 y 0
         res {}
         raw {}
         start nil]
    (if (empty? chars)
      (let [near-start (set (keys (filter (fn [[k v]] (contains? v start)) res)))]
        {:start start :data (assoc res start near-start) :raw raw})
      (let [c (first chars)
            res (if (= c \newline)
                  res
                  (assoc res [x y] (case c
                                     \F #{[x (inc y)] [(inc x) y]}
                                     \L #{[x (dec y)] [(inc x) y]}
                                     \- #{[(dec x) y] [(inc x) y]}
                                     \| #{[x (inc y)] [x (dec y)]}
                                     \7 #{[(dec x) y] [x (inc y)]}
                                     \J #{[x (dec y)] [(dec x) y]}
                                     nil)))
            start (if (= c \S) [x y] start)
            [new-x new-y] (if (= c \newline) [0 (inc y)] [(inc x) y])
            raw (assoc raw [new-x new-y] c)]
        (recur (rest chars) new-x new-y res raw start)))))

(defn solve1 [{:keys [start data]}]
  (loop [prev start
         cur (first (get data start))
         steps 1]
    (if (= cur start)
      (int (/ steps 2))
      (recur cur
             (first (filter #(not= prev %) (get data cur)))
             (inc steps)))))

(defn get-loop-tiles [{:keys [start data]}]
  (loop [prev start
         cur (first (get data start))
         res #{start}]
    (if (= cur start)
      res
      (recur cur
             (first (filter #(not= prev %) (get data cur)))
             (conj res cur)))))

(defn write-cleaned-input [found]
  (let [input (get-input)
        loop-tiles (get-loop-tiles input)
        max-x (apply max (map first (keys (:data input))))
        max-y (apply max (map second (keys (:data input))))
        data (for [y (range 0 (inc max-y))]
               (for [x (range 0 (inc max-x))]
                 (if (contains? loop-tiles [x y]) "|"
                     (if (contains? found [x y]) "x" " "))))]
    (pprint/pprint (map #(apply str %) data))
    (println (cset/intersection found loop-tiles))
    ))

(defn solve2 [input]
  (let [loop-tiles (get-loop-tiles input)
        max-x (apply max (map first (keys (:data input))))
        max-y (apply max (map second (keys (:data input))))
        in-range (fn [[x y]] (and (<= 0 x max-x) (<= 0 y max-y)))
        get-surrounding (fn [[x y]]
                          (set (for [xx (range (dec x) (+ 2 x))
                                     yy (range (dec y) (+ 2 y))
                                     :when (not= [x y] [xx yy])]
                                 (if (in-range [xx yy])
                                   [xx yy]
                                   [nil nil]))))
        candidates (cset/difference (set (keys (:data input))) loop-tiles)
        print-location (fn [[x y]]
                         (println "=========")
                         (pprint/pprint (for [yy (range (dec y) (+ 2 y))]
                                          (for [xx (range (dec x) (+ 2 x))]
                                            (get (:raw input) [xx yy]))))
                         (pprint/pprint (for [yy (range (dec y) (+ 2 y))]
                                          (for [xx (range (dec x) (+ 2 x))]
                                            (contains? loop-tiles [xx yy])))))
        ]
    (loop [remaining (set (rest candidates))
           to-check (conj #{} (first candidates))
           checked #{}
           surround #{}
           res #{}]
      (if (and (empty? to-check)
               (empty? remaining))
        (do
          ;;(write-cleaned-input res)
          (count res))
        (if (empty? to-check)
          (let[within-loop (not-any? #(= [nil nil] %) surround)
               res (if within-loop (cset/union res checked) res)
               remaining (cset/difference remaining checked)
               to-check (if (empty? remaining) #{} (conj #{} (first remaining)))
               a (println (count checked))
               ;;a (println "Found" (count res) checked)
               ;; a (if (= 1 (count checked))
               ;;     (print-location (first checked)))
               ]
            (recur (set (rest remaining)) to-check #{} #{} res))
          (let [tmp-to-check (apply cset/union (map get-surrounding to-check))
                new-checked (cset/union checked to-check)
                tmp-to-check (cset/difference tmp-to-check new-checked)
                ;; get the loop and outside
                tmp-surround (set (filter #(or (= [nil nil] %) (contains? loop-tiles %))
                                          tmp-to-check))
                new-surround (cset/union surround tmp-surround)
                ;; get the ones we should check next (non-loop, not outside)
                new-to-check (cset/difference tmp-to-check tmp-surround)
                ]
            (recur remaining new-to-check new-checked new-surround res)))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
