(ns day3.core)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]    
    (for [line lines]
      (str/split line #""))))


(defn transpose [m]
  (apply mapv vector m))

(defn get-commons [f data]
  (let [d (transpose data)
        commons (for [tmp (map frequencies d)
                      :let [zeros (get tmp "0" 0)
                            ones (get tmp "1" 0)]]
                  (if (f ones zeros) ["1" "0"] ["0" "1"]))]
    commons))

(defn solve1 [data]
  (let [commons (get-commons > data)
        gamma (Integer/parseInt (apply str (map first commons)) 2)
        epsilon (Integer/parseInt (apply str (map second commons)) 2)]
    (* gamma epsilon)))


(defn calc2 [f pos data]
  (if (= 1 (count data))
    (Integer/parseInt (apply str (first data)) 2)    
    (let [common (nth (map first (get-commons f data)) pos)
          new-data (filter #(= common (get % pos)) data)]
      (recur f (inc pos) new-data))))

(defn solve2 [data]
  (let [oxygen (calc2 >= 0 data)
        co2 (calc2 < 0 data)]
    (* oxygen co2)))

(let [input1 (get-input "input1.txt")
      input2 (get-input "input1.txt")
      part1 (solve1 input1)
      part2 (solve2 input2)]
  (println "Part1: " part1)
  (println "Part2: " part2))
