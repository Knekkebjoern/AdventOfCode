(ns day4.core)
(require '[clojure.string :as str])


(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        draws (map #(Integer/parseInt %) (str/split (first lines) #","))
        boards-txt (partition 5 (filter #(not (empty? % )) (rest lines)))
        boards (for [board boards-txt]
                 (for [row board
                       :let [values (str/split (str/trim row) #"\s+")]]
                   (map #(Integer/parseInt %) values)))]
    {:draws draws :boards boards}))

(defn winner-board? [drawn board]
  (let [drawn-set (set drawn)
        matches (for [line board
                      :when (clojure.set/superset? drawn-set (set line))]
                  line)]
    (not (empty? matches))))

(defn get-unmarked [draws board]
  (clojure.set/difference (set (flatten board)) (set draws)))

(defn get-winners [data]
  (let [boards (:boards data)
        draws (:draws data)
        indexed-boards (for [board boards
                             :let [transposed (transpose board)]]
                         (concat board transposed))]
    (loop [drawn (vec (take 5 draws))
           to-draw (drop 5 draws)
           boards indexed-boards
           winners []]
      (if (or (empty? to-draw)
              (empty? boards))
        winners
        (let [new-drawn (conj drawn (first to-draw))
              new-winners (filter #(winner-board? new-drawn %) boards)
              new-boards (clojure.set/difference (set boards) (set new-winners))]
          (recur new-drawn
                 (rest to-draw) 
                 new-boards
                 (conj winners {:drawn new-drawn :boards new-winners})
                 ))))))

(defn solve1 [f data]
  (let [winners (get-winners data)
        winner (f (filter #(not (empty? (:boards %))) winners))
        drawn (:drawn winner)
        unmarked (get-unmarked drawn (first (:boards winner)))]              
          (* (reduce + unmarked) (last drawn))))


(let [input1 (get-input "input1.txt")
      part1 (solve1 first input1)
      part2 (solve1 last input1)]
  (println "Part1: " part1)
  (println "Part2: " part2)
  )
