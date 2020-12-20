(ns day12.core
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (for [line (str/split (slurp filename) #"\n")
        :let [[_ command arg] (re-matches #"^([A-Z]{1})(\d+)$" line)]]
    [(keyword command) (Integer/parseInt arg)] ))

(defn solve1 [commands loc dir]
  (loop [commands commands loc loc dir dir]
    (if (empty? commands)
      loc
      (let [[command arg] (first commands)
            newcommand  (if (= command :F)
                          (let [i (mod (/ dir 90) 4)]
                            (nth [:E :S :W :N] i))
                          command)
            n (if (contains? #{:S :W} newcommand) -1 1)
            newloc (cond
                     (contains? #{:N :S} newcommand)
                     (map + loc [0 (* n arg)])
                     (contains? #{:E :W} newcommand)
                     (map + loc [(* n arg) 0])
                     :else loc)
            newdir (case newcommand
                     :L (- dir arg)
                     :R (+ dir arg)
                     dir)]

        (recur (rest commands) newloc newdir)))))

(defn rotate [[x y] degrees]
  (let [radians (Math/toRadians degrees)]
    [(int (Math/round (- (* x (Math/cos radians)) (* y (Math/sin radians)))))
     (int (Math/round (+ (* x (Math/sin radians)) (* y (Math/cos radians)))))]))

(defn solve2 [commands ship waypoint]
  (loop [commands commands ship ship waypoint waypoint]
    (if (empty? commands)
      ship
      (let [[command arg] (first commands)
            newwaypoint (case command
                          :N (map + waypoint [0 arg])
                          :S (map - waypoint [0 arg])
                          :E (map + waypoint [arg 0])
                          :W (map - waypoint [arg 0])
                          :R (rotate waypoint (- arg))
                          :L (rotate waypoint arg)
                          waypoint
                          )
            newship (case command
                      :F (map + ship (map #(* arg %) newwaypoint))
                      ship)]
        (recur (rest commands) newship newwaypoint)))))



(defn -main
  [& args]
  (let [input (get-input "input.txt")
        part1 (solve1 input [0 0] 0)
        part2 (solve2 input [0 0] [10 1])]
    (println "Part1: " (apply + (map #(Math/abs %) part1)))
    (println "Part2: " (apply + (map #(Math/abs %) part2)))))
