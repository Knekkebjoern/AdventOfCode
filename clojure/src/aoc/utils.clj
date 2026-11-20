(ns aoc.utils)

(defn drop-indices
  "Returns coll without the elements indexed by `indices`.
(drop-indices [1 2 3 4 5 6] [0 1 4]) ;; => [3 4 6]
"
  [coll indices]
  {:pre [(sequential? coll)
         (sequential? indices)
         (every? int? indices)]}
  (let [indices (set indices)]
    (keep-indexed (fn [i n]
                    (if (indices i) nil n) ; set type lookup returns nil or `i`
                    )
                  coll)))
