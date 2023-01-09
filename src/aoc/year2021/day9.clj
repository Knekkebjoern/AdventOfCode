(ns aoc.year2021.day9)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data  (loop [lines lines y 0 res {}]
                (if (empty? lines)
                  res
                  (let [line (first lines)
                        d (apply merge
                                 (for [x (range (count line))
                                       :let [v (Integer/parseInt (str (nth line x)))]]
                                   {[x y] v}))]
                    (recur (rest lines) (inc y) (merge res d)))))]
    data))

(defn get-ordinal-mask [x y d]
  (for [xx (range (- x d) (+ x d 1))
        yy (range (- y d) (+ y d 1))
        :when (and (<= 0 xx)
                   (<= 0 yy)
                   (or (= xx x)
                       (= yy y))
                   (not (= [xx yy] [x y])))]
    [xx yy]))

(defn get-neighbors [[x y] data]
  (select-keys data (get-ordinal-mask x y 1)))

(defn is-minimum [[x y] data]
  (let [v (get data [x y])
        neighbors (get-neighbors [x y] data)]
    (and (not (empty? neighbors))
         (every? #(> % v) (vals neighbors)))))

(defn solve1 [data]
  (let [minima (for [k (keys data)
                     :when (is-minimum k data)]
                 k)
        vs (vals (select-keys data minima))]
    (reduce + (map inc vs))))

(defn get-surrounding [[x y] data]
  (let [mask (set (get-ordinal-mask x y 1))]
    (clojure.set/intersection data mask)))

(defn find-basins [data cur res]
  (if (empty? data)
    (conj res cur)
    (let [candidates (if (empty? cur) #{(first data)} cur)
          neighbors (set (apply concat (map #(get-surrounding % data) candidates)))
          new-data (clojure.set/difference data neighbors)
          new-cur (if (empty? neighbors) #{} (set (concat cur neighbors)))
          new-res (if (empty? neighbors) (conj res cur) res)]
      (recur new-data new-cur new-res))))

(defn solve2 [data]
  (let [basins (find-basins (set (keys
                                  (filter (fn [[_ v]] (not (= v 9))) data)))
                            #{} [])]
    (reduce * (take 3 (reverse (sort (map count basins)))))))

(defn solve []
  (let [input (get-input"inputs/2021/day9.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
