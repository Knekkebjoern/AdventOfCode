(ns day5.core
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

(defn -main
  [& args]
  (let [data (map parse (get-input "input.txt"))
        ids (set (map #(get % :id) data))]
    (println "Max seat id:" (apply max ids))
    (println "My seat id:" (filter #(and (.contains ids (inc %))
                                         (.contains ids (dec %)))
                                   (sort (set/difference (set (range 1024)) ids))))))
