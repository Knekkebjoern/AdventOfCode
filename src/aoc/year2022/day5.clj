(ns aoc.year2022.day5)
(require '[clojure.set :as set])
(require '[clojure.string :as str])

(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        moves (for [line lines
                    :when (re-matches #"^move.*" line)
                    :let [m (re-matches #"^move (\d+) from (\d+) to (\d+).*" line)]]
                (map #(Integer/parseInt %) (rest m)))
        stacks (transpose
                (for [line lines
                      :when (re-matches #"^.*\[[A-Z]{1}\].*" line)
                      :let [ps (map first
                                    (map (fn [xs]
                                           (filter #(not (contains? #{\s \[ \]} %))
                                                   xs))
                                         (partition 4 4 " " line)))]]
                  ps))
        stacks (for [s stacks]
                 (drop-while #(= \space %) s))
        stacks (loop [i 1 xs stacks res {}]
                 (if (empty? xs)
                   res
                   (recur (inc i) (rest xs) (assoc res i (first xs)))))]

    [stacks moves]))

(defn solve1 [[stacks moves] rev]
  (if (empty? moves)
    (apply str (for [i (sort (keys stacks))]
                (first (get stacks i))))
    (let [[n from to] (first moves)
          from-s (get stacks from)
          crates (if rev
                   (reverse (take n from-s))
                   (take n from-s))
          new-from-s (drop n from-s)
          to-s (get stacks to)
          new-to-s (flatten (conj to-s crates))
          new-stacks (assoc stacks from new-from-s to new-to-s)
          ]
      (recur [new-stacks (rest moves)] rev))))

(defn solve []
  (let [input (get-input "inputs/2022/day5.txt")
        part1 (solve1 input true)
        part2 (solve1 input false)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
