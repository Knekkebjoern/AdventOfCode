(ns aoc.year2024.day6
  (:require
   [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 6))

(def input-filename "inputs/2024/day6.txt")
;;(def input-filename "inputs/2024/day6-test.txt")

(defn get-input []
  (let [lines (io/split-lines (io/read-file input-filename))]
    (loop [lines lines
           y 0
           res {}]
      (if (empty? lines)
        res
        (let [tmp (apply merge (map-indexed (fn [i c] {[i, y] c}) (first lines)))
              res (merge res tmp)]
          (recur (rest lines) (inc y) res))))))

(defn is-guard? [c]
  (= \^ c))

(defn is-obstacle? [floor-map loc]
  (= \# (get floor-map loc)))

(defn get-direction [c]
  (case c
    \^ :north))

(defn outside-bounds? [floor-map loc]
  (not (get floor-map (vec loc) false)))

(defn turn-guard [dir]
  (case dir
    :north :east
    :south :west
    :east :south
    :west :north))

(defn loc-in-front [loc dir]
  (vec (case dir
         :north (map + loc [0 -1])
         :south (map + loc [0 1])
         :east (map + loc [1 0])
         :west (map + loc [-1 0]))))

(defn move-or-turn-guard [floor-map loc dir]
  (let [next-loc (loc-in-front loc dir)]
    (if (is-obstacle? floor-map next-loc)
      [loc (turn-guard dir)]
      [next-loc dir])))

(defn solve1 [floor-map]
  (let [[guard-loc c] (first (filter (fn [[loc c]] (is-guard? c)) floor-map))
        guard-dir (get-direction c)]
    (loop [guard-loc guard-loc
           seen-locs #{guard-loc}
           guard-dir guard-dir]
      (if (outside-bounds? floor-map guard-loc)
        (count seen-locs)
        (let [;;_ (println "guard-loc" guard-loc "guard-dir" guard-dir "seen-locs" (count seen-locs))

              seen-locs (conj seen-locs guard-loc)
              [guard-loc guard-dir] (move-or-turn-guard floor-map guard-loc guard-dir)]
          (recur guard-loc seen-locs guard-dir))))))

(defn is-loop? [floor-map loc dir]
  (loop [guard-loc loc
         guard-dir dir
         seen #{}]
    (cond
      (contains? seen [guard-loc guard-dir]) true
      (outside-bounds? floor-map guard-loc) false
      :else (let [seen (conj seen [guard-loc guard-dir])
                  [guard-loc guard-dir] (move-or-turn-guard floor-map guard-loc guard-dir)]
              (recur guard-loc guard-dir seen)))))

(defn solve2 [floor-map]
  ;; Starting location and direction
  (let [[starting-guard-loc c] (first (filter (fn [[_ c]] (is-guard? c)) floor-map))
        starting-guard-dir (get-direction c)]
    (loop [guard-loc starting-guard-loc
           guard-dir starting-guard-dir
           obstacles #{}]
      (if (outside-bounds? floor-map guard-loc)
        (count obstacles)
        (let [;; place an obstacle at the location the guard would path to next
              obstacle-loc (loc-in-front guard-loc guard-dir)
              new-floor-map (assoc floor-map obstacle-loc \#)
              ;; Add the location if it's a loop and not an obstacle in the original map
              obstacles (if (and (not (= starting-guard-loc guard-loc))
                                 (not (is-obstacle? floor-map obstacle-loc))
                                 (is-loop? new-floor-map starting-guard-loc starting-guard-dir))
                          (conj obstacles obstacle-loc)
                          obstacles)
              ;; Move the guard along the original path
              [new-guard-loc new-guard-dir] (move-or-turn-guard floor-map guard-loc guard-dir)]
          (recur new-guard-loc new-guard-dir obstacles))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))
(defn status [] "*")
