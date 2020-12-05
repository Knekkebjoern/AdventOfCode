(ns day3.core
  (:gen-class))

(require '[clojure.string :as str])

(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (transpose (str/split (slurp filename) #"\n")))

(defn solve [data step-right step-down]
  (let [end (count (first data))]
    (loop [data (cycle data) pos 0 cnt 0]
      (if (>= pos end)
        cnt
        (recur (drop step-right data)
               (+ pos step-down)
               (if (= \# (nth (first data) pos))
                 (inc cnt)
                 cnt))))))

(defn -main
  [& args]
  (let [data (get-input "input.txt")]
    (println "Trees hit in part 1:" (solve data 3 1))
    (println "Trees hit in part 2:" (reduce * (for [[r d] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
                                                (solve data r d))))))
