(ns year_2021.day14)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        [pattern _ & mappings] lines
        mappings (for [m mappings
                       :let [[ab c] (str/split m #" -> ")
                             [a b]  (for [x ab] x)
                             c (first (for [x c] x))]]
                   {[a b] (list [a c] [c b])})]
    {:pattern pattern :mappings (apply merge mappings)}))

(defn get-counts [pairs]
  (reduce-kv (fn [res k v]
               (let [nv (bigint (/ v 2))]
                 (assoc res k (if (even? v) nv (inc nv)))))
             {}
             (apply (partial merge-with +)
                    (apply concat (for [[[a b] v] pairs]
                                    (merge-with + [{a v} {b v}]))))))

(defn get-answer [counts]
  (let [min-max (reduce-kv (fn [res k v]
                             {:min (if (< v (:min res)) v (:min res))
                              :max (if (> v (:max res)) v (:max res))})
                           {:min (bigint (* Integer/MAX_VALUE Integer/MAX_VALUE)) :max 0}
                           counts)]
    (- (:max min-max) (:min min-max))))

(defn solve1 [mappings pairs iters]
  (if (zero? iters)
    (get-answer (get-counts pairs))
    (let [new-pairs  (apply (partial merge-with +)
                            (for [[pair pair-v] pairs
                                  new-pair (get mappings pair [])]
                              {new-pair pair-v}))]
          
      (recur mappings new-pairs (dec iters)))) )

(defn main []
  (let [input1 (get-input "inputs/day14_input1.txt")
        part1  (solve1 (:mappings input1)
                       (frequencies (partition 2 1 (:pattern input1)))
                       10)
        part2  (solve1 (:mappings input1)
                       (frequencies (partition 2 1 (:pattern input1)))
                       40)]
    (println "Part1: " part1)
    (println "Part2: " part2)))

(main)
