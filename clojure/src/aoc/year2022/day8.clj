(ns aoc.year2022.day8)
(require '[clojure.string :as str])

(defn get-input [filename]
  (loop [lines (str/split (slurp filename) #"\n") row 0 cols 0 data {}]
    (if (empty? lines)
      {:data data :rows row :cols cols}
      (let [rowdata (apply merge
                           (for [col (range (count (first lines)))]
                             {[col row] (Integer/parseInt (str (nth (first lines) col)))}))
            newdata (apply merge data rowdata)]
       (recur (rest lines) (inc row) (count (first lines)) newdata)))))

(defn get-los [[x y] rows cols]
  {:west (for [i (range (dec x) -1 -1)] [i y])
   :east (for [i (range (inc x) cols)] [i y])
   :north (for [i (range (dec y) -1 -1)] [x i])
   :south (for [i (range (inc y) rows)] [x i])
   })

(defn visible? [[x y] data]
  (let [m (:data data)
        height (get m [x y])
        los (get-los [x y] (:rows data) (:cols data))
        vis (for [dir (keys los)
                  :let [heights (for [i (get los dir)]
                                  (get m i))]]
              (empty? (filter #(>= % height) heights)))]
    (some true? vis)))

(defn solve1 [data]
  (let [visible (for [x (range (:rows data))
                      y (range (:cols data))
                      :when (visible? [x y] data)]
                  [x y])]
    (count visible)))

(defn count-trees [heights height]
  (loop [heights heights res []]
    (if (empty? heights)
      res
      (if (>= (first heights) height)
        (conj res (first heights))
        (recur (rest heights) (conj res (first heights)))))))

(defn score [[x y] data]
  (let [height (get (:data data) [x y])
        los (get-los [x y] (:rows data) (:cols data))
        heights (for [dir (keys los)
                      :let [heights (for [coord (get los dir)]
                                      (get (:data data) coord))]]
                  (count-trees heights height))
        totals (map count heights)]
    (apply * totals)))

(defn solve2 [data]
  (let [scores (for [x (range (:rows data))
                     y (range (:cols data))]
                 (score [x y] data))]
    (apply max scores)))

(defn solve []
  (let [input (get-input "inputs/2022/day8.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
