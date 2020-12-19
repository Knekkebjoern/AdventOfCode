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

(defn solve2 [commands loc dir]
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

        (recur (rest commands) newloc newdir))))
  )

(defn -main
  [& args]
  (let [input (get-input "input.txt")
        part1 (solve1 input [0 0] 0)
        a (prn part1)]
    (println "Part1: " (apply + (map #(Math/abs %) part1)))))
