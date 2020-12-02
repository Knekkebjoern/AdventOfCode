(ns day1.core
  (:gen-class))
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map #(Integer/parseInt %) lines)]
    data))

(defn find-parts [coll target cnt]
  (if (= 1 cnt)
    (if (.contains coll target)
      [target]
      nil)
    (loop [c coll]
      (if (empty? c)
        []
        (let [v (first c)
              new-target (- target v)
              res (find-parts (rest c) new-target (dec cnt))
              ]
          (if (empty? res)
            (recur (rest c))
            (conj res v ))))))
  )

(defn solve [coll target count]
  (find-parts (sort coll) target count))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [part1 (solve (get-input "input.txt") 2020 2)
        part2 (solve (get-input "input.txt") 2020 3)]
    (println "Part 1:" part1 "Answer:" (apply * part1))
    (println "Part 2:" part2 "Answer:" (apply * part2))
    )

  )
