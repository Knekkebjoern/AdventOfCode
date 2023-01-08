(ns aoc.year2019.day6
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-orbits [filename]
  (let [lines (str/split (slurp filename) #"\n")
        pairs (map #(reverse (str/split %1 (re-pattern "\\)"))) lines)
        orbits (apply hash-map (flatten pairs))]
    orbits))

(defn find-indirect [k orbits res]
  (let [v (get orbits k)]
    (if (zero? (compare "COM" v))
      (cons "COM" res)
      (recur v orbits (cons v res)))))

(defn flatten-orbits [orbits]
  (into {} (for [k (keys orbits)
                 :let [n (find-indirect k orbits [])]]
             [k n])))

(defn get-common-orbits [a b]
   (set/intersection (set a) (set b)))

(defn solve [& args]
  (let [orbits (get-orbits "inputs/2019/day6.txt")
        flat-orbits (flatten-orbits orbits)
        cnts (for [x (vals flat-orbits)]
               (count x))
        res1 (reduce + cnts)]
    (let [you-orbit (get flat-orbits "YOU")
          san-orbit (get flat-orbits "SAN")
          common-orbits (get-common-orbits (reverse you-orbit)
                                   (reverse san-orbit))
          cnts (for [common common-orbits
                     :let [you-path (drop-while #(not (zero? (compare common %1)))
                                                you-orbit)
                           san-path (drop-while #(not (zero? (compare common %1)))
                                                san-orbit)]]
                 {:common common
                  :you-path you-path
                  :san-path san-path
                  :transfers (- (+ (count you-path) (count san-path)) 2)}
                 )]
      {:part1 res1
       :part2 (:transfers (first (sort-by :transfers cnts)))})))

(defn status []
  "*")
