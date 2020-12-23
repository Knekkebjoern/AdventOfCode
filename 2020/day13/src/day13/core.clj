(ns day13.core
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (for [line (str/split (slurp filename) #"\n")
        :let [[_ command arg] (re-matches #"^([A-Z]{1})(\d+)$" line)]]
    [(keyword command) (Integer/parseInt arg)] ))

(def timestamp 1008169)
(def input [29 :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x 41 :x :x :x 37 :x :x :x :x :x 653 :x :x :x :x :x :x :x :x :x :x :x :x 13 :x :x :x 17 :x :x :x :x :x 23 :x :x :x :x :x :x :x 823 :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x :x 19])

;;(def timestamp 939)
;;(def input [7 13 :x :x 59 :x 31 19])
(def input [17 :x 13 19])
;;(def input [67 7 :x 59 61])

(defn solve1 [time ids]
  (let [mods (map #(mod time %) ids)]
    (nth ids(.indexOf mods (apply max mods)))))

(defn divisors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn divides? [m n] (zero? (rem m n)))
(defn prime? [n] (and (< 1 n) (not-any? #(divides? n %) (range 2 n))))

(defn solve3 [ids]
  (let [targets (filter #(not (nil? %)) (map-indexed (fn [i id]
                                                       (if (= :x id)
                                                         nil
                                                         (mod (- id i) id)))
                                                     ids))
        a (prn "------")
        a (prn "targets" targets)
        divisors (set (flatten (map divisors targets)))
        a (prn "divisors" divisors)
        a (prn "hmm" (apply * divisors))
        primes (filter prime? divisors)
        a (prn "primes" primes)
        res (apply * primes)
        a (prn "res" res)
        ]
    targets))

(solve3 input)

(defn -main
  [& args]
  (let [bus_id (solve1 timestamp (filter #(not= :x %) input))
        wait (- bus_id (mod timestamp bus_id))
        ;part1 (* bus_id wait)
        part2 (solve2 input)]
    ;(println "Part1: " part1)
    (println "Part2: " part2)))
