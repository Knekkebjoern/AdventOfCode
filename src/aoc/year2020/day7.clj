(ns aoc.year2020.day7
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-input [filename]
  (apply merge
   (for [line (str/split (slurp filename) #"\n")]
     (let [[_ k vs] (re-matches #"^(.*?) bags contain (.*?)\.$" line)
           vs (for [v (str/split vs #",")
                    :let [[_ a b] (re-find #"(\d+)\s+(.*?)\s+bag" v)]
                    :when (not (nil? b))]
                {b (Integer/parseInt a)})]
       {k (apply merge vs)})))) ; {drab purple {shiny indigo 1, striped yellow 4}}

(defn can-hold [data ts]
  (set (for [[k v] data :when (some true? (map #(contains? v %) ts))] k)))

(defn solve-fn [data tsin]
  (loop [ts tsin]
    (let [newts (can-hold data ts)]
      (if (empty? (set/difference newts ts))
        newts
        (recur (set/union ts newts))))))

(defn bags-required [data tsin]
  (let [d (get data tsin)]
    (if (empty? d)
      0
      (reduce + (for [[k v] d] (+ v (* v (bags-required data k))))))))

(defn solve
  [& args]
  (let [data (get-input "inputs/2020/day7.txt")]
    {:part1 (count (solve-fn data ["shiny gold"]))
     :part2 (bags-required data "shiny gold")}))

(defn status [] "*")
