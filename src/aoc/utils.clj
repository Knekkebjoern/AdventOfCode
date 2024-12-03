(ns aoc.utils)

(defn drop-indices
  "Returns coll without the elements indexed by `is`.
(drop-indices [1 2 3 4 5 6] [0 1 4]) ;; => [3 4 6]
"
  [coll is]
  {:pre [(sequential? coll)
         (sequential? is)
         (every? int? is)]}
  (let [is (set is)]
    (loop [coll coll
           i 0
           res []]
      (if (empty? coll)
        res
        (let [res (if (is i)
                    res
                    (conj res (first coll)))]
          (recur (rest coll) (inc i) res))))))
