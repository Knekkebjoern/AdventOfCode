(ns aoc.year2020.day17
  (:gen-class))

(def test-input [[\. \# \.]
                 [\. \. \#]
                 [\# \# \#]])

(def input
  [[\. \# \. \# \# \. \. \#]
   [\. \. \. \. \# \. \# \#]
   [\# \# \. \# \# \# \. \.]
   [\. \# \. \# \. \# \# \#]
   [\# \. \# \. \. \. \. \.]
   [\. \# \. \. \# \# \# \.]
   [\. \# \# \# \# \# \. \#]
   [\# \. \. \# \# \# \# \.]])

(defn to-state [input z w]
  (loop [input input y 0 state {}]
    (if (empty? input)
      state
      (let [row (apply merge
                       (map-indexed
                        (fn [x v] {[x y z w] (if (= v \.) :inactive :active)})
                        (first input)))
            newstate (merge state row)]
        (recur (rest input) (inc y) newstate)))))

(defn get-neighbors [[x y z w]]
  (set (for [xx (range (dec x) (+ 2 x))
             yy (range (dec y) (+ 2 y))
             zz (range (dec z) (+ 2 z))
             ww (range (dec w) (+ 2 w))
             :when (not= [xx yy zz ww] [x y z w])]
         [xx yy zz ww])))

(defn next-cycle [values]
  (into {} (for [[coord state] values
                :let [neighbors (get-neighbors coord)
                      neighbor-states (for [k neighbors
                                            :let [v (get values k :inactive)]] v)
                      active-neighbor-count (count (filter #(= :active %) neighbor-states))
                      newstate (if (or (and (= state :inactive)
                                            (= 3 active-neighbor-count))
                                       (and (= state :active)
                                            (contains? #{2 3} active-neighbor-count)))
                                 :active
                                 :inactive)
                      ]]
            {coord newstate})))

(defn transpose [m]
  (apply mapv vector m))

(defn add-padding [state]
  (let [coords (keys state)
        transposed (transpose coords)
        [xrange yrange zrange wrange] (for [tmp transposed]
                                        (range (dec (apply min tmp))
                                               (+ 2 (apply max tmp))))]
    (into {} (for [x xrange y yrange z zrange w wrange
                  :let [v (get state [x y z w] :inactive)]]
              {[x y z w] v}))))

(defn solve1 [input]
  (let [state (to-state input 0 0)]
    (loop [state state iter 0]
      (if (= 6 iter)
        state
        (let [padded-state (add-padding state)]
          (recur (next-cycle padded-state) (inc iter)))))))

(defn solve
  [& args]
  (let [result1 (solve1 input)
        active (count (filter #(= :active %) (vals result1)))]
    {:part1 nil :part2 active}))

; Missing part1 somehow
(defn status [] "!")
