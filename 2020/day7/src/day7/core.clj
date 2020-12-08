(ns day7.core
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

(defn solve [data tsin]
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


(defn -main
  [& args]
  (let [data (get-input "input.txt")]
    (println "Part 1:" (count (solve data ["shiny gold"])))
    (println "Part 2:" (bags-required data "shiny gold"))))

(-main)
