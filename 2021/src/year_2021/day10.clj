(ns year_2021.day10)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]
    lines))

(defn closes? [open close]
  (case [open close]
    [\( \)] true
    [\[ \]] true
    [\< \>] true
    [\{ \}] true
    false))

(defn check-line [line]
  (loop [data line stack [] err nil]
    (let [k (first data)]
      (cond 
        (not (nil? err)) {:invalid err}
        (empty? data) (if (empty? stack) {:valid true} {:incomplete stack})      
        :else  (case k
                 (\( \< \{ \[) (recur (rest data) (conj stack k) nil)
                 (\) \> \} \]) (if (closes? (last stack) k)
                                 (recur (rest data) (into [] (butlast stack)) nil)
                                 (recur data stack k)))        
        ))))

(def scores {\) 3
             \] 57
             \} 1197
             \> 25137})

(defn solve1 [data]
  (let [res (for [line data]
              (check-line line))        
        invalid (map :invalid (filter #(contains? % :invalid) res))
        scores (for [k invalid]
                 (get scores k 0))]
    (reduce + scores)))

(def complete-scores {\( 1
                      \[ 2
                      \{ 3
                      \< 4})

(defn complete-and-score-stack [stack score]
  (if (empty? stack)
    score
    (let [k (last stack)]
      (recur (butlast stack) (+ (* 5 score) (get complete-scores k))))))

(defn solve2 [data]
  (let [res (for [line data]
              (check-line line))        
        incomplete (map :incomplete (filter #(contains? % :incomplete) res))
        scores (sort (for [stack incomplete]
                      (complete-and-score-stack stack 0)))
        median (nth scores (quot (count scores) 2))]
    median))

(let [input1 (get-input"inputs/day10_input1.txt")      
      part1 (solve1 input1)
      part2 (solve2 input1)
      ]
  (println "Part1: " part1)
  (println "Part2: " part2))

