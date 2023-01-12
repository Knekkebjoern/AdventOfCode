(ns aoc.year2022.day14)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn get-range [a b]
  (if (<= a b)
    (range a (inc b))
    (range b (inc a))))

(defn parse-input [line]
  ;; Return a map of coordinate keys and :rock values
  ;; Partition (1 2 3 4) => ((1 2) (2 3) (3 4))
  (let [coords (apply concat
                      (let [pairs (partition 2 1 (str/split line #" -> "))]
                        (for [[a b] pairs
                              :let [[ax ay] (map #(Integer/parseInt %) (str/split a #","))
                                    [bx by] (map #(Integer/parseInt %) (str/split b #","))
                                    xs (get-range ax bx)
                                    ys (get-range ay by)]]
                          (set (for [x xs y ys]
                                 [x y])))))]
    (zipmap coords (repeat :rock))))

(defn get-input [filename]
  (apply merge
         (for [line (str/split (slurp filename) #"\n")]
           (parse-input line))))

(defn get-next-pos [state [x y]]
  (first
   (filter
    (fn [pos]
      (nil? (get state pos)))
    [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]])))

(defn solve1 [state]
  (let [max-y (apply max (map second (keys state)))]
    (loop [state state curr [500 0]]
      (let [next-pos (get-next-pos state curr)]
        (cond
          (nil? next-pos) (recur (assoc state curr :sand) [500 0])
          (> (second next-pos) max-y) (count (filter #(= :sand %) (vals state)))
          :default (recur state next-pos))))))

(defn solve2 [state]
  (let [max-y (apply max (map second (keys state)))
        floor-y (+ 2 max-y)]
    (loop [state state curr [500 0]]
      (let [next-pos (get-next-pos state curr)]
        (cond
          (and (nil? next-pos)
               (= curr [500 0])) (inc (count (filter #(= :sand %) (vals state))))
          (or (nil? next-pos)
              (= floor-y (second next-pos))) (recur (assoc state curr :sand) [500 0])
          :default (recur state next-pos))))))

(defn solve []
  (let [input (get-input "inputs/2022/day14.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
