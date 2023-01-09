(ns aoc.year2022.day10)
(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]
    (for [line lines]
      (if-let [[_ d] (re-matches #"^addx (\-*\d+)$" line)]
        [:addx (Integer/parseInt d)]
        (if (= "noop" line)
          [:noop]
          :unknown)))))

(defn run [data]
  (loop [ops data cycle 1 register 1 history {}]
    (if (empty? ops)
      history
      (let [op (first ops)
            newcycle (case (first op)
                       :noop (inc cycle)
                       :addx (+ 2 cycle))
            newregister (case (first op)
                          :noop register
                          :addx (+ register (second op)))
            newhistory (case (first op)
                         :noop (assoc history cycle register)
                         :addx (assoc history cycle register (inc cycle) register))]
        (recur (rest ops) newcycle newregister newhistory)))))

(defn solve1 [data]
  (let [history (run data)]
    (reduce + (for [x [20 60 100 140 180 220]]
                (* x (get history x))))))

(defn solve2 [data]
  (let [history (run data)
        crt (zipmap (range 1 241)
                    (take 240 (cycle (range 0 40))))
        rows (partition 40
                        (for [t (range 1 241)
                              :let [register (get history t)
                                    sprite (get crt t)
                                    delta (Math/abs (- register sprite))]]
                          (if (<= delta 1) \# \.)))]
    (for [row rows
          :let [s (apply str row)]]
      s)))

(defn solve []
  (let [input (get-input "inputs/2022/day10.txt")
        part1 (solve1 input)
        part2 (pprint/pprint (solve2 input))]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
