(ns aoc.year2020.day15
  (:gen-class))

(def input {6 1 4 2 12 3 1 4 20 5 0 6 16 7})

(defn solve1 [state target]
  (loop [turn (inc (count (keys state))) n 0 state state]
    (if (= turn target)
      n
      (let [previous (get state n nil)
            nextnum (if (nil? previous)
                      0
                      (- turn previous))
            newstate (assoc state n turn)]
        (recur (inc turn) nextnum newstate)))))

(defn solve
  [& args]
  (let [result1 (solve1 input 2020)
        result2 (solve1 input 30000000)]
    {:part1 result1 :part2 result2}))

(defn status [] "*")
