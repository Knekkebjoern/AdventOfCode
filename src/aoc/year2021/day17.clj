(ns aoc.year2021.day17)
(require '[clojure.string :as str])

(defn trajectory [startx starty startxv startyv txmin txmax tymin tymax]
  (loop [x startx y starty xv startxv yv startyv maxy 0]
    (cond
      (< y tymin)  nil
      (or (and (pos? xv)
               (> x txmax))
          (and (neg? xv)
               (< x txmin))) nil
      (and (>= x txmin) (<= x txmax)
           (>= y tymin) (<= y tymax)) {:velocity [startxv startyv] :maxy maxy}
      :else (let [new-x (+ x xv)
                  new-y (+ y yv)
                  new-xv (cond (> xv 0) (dec xv)
                               (< xv 0) (inc xv)
                               :else 0)
                  new-yv (dec yv)
                  new-maxy (if (> new-y maxy) new-y maxy)]
              (recur new-x new-y new-xv new-yv new-maxy)))))

(defn solve []
  ;; target area: x=175..227, y=-134..-79
  (let [
        xrange (range 0 228)
        yrange (range -134 1000)
        results (filter some?
                        (for [xv xrange yv yrange]
                          (trajectory 0 0 xv yv 175 227 -134 -79)))
        maxy (apply max (map :maxy results))
        velocities (set (map :velocity results))]
    {:part1 maxy :part2 (count velocities)}))

(defn status [] "*")
