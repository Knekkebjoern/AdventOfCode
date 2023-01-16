(ns aoc.year2022.day15)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[clojure.set :as set])

(defn manhattan-distance [[ax ay] [bx by]]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))

(defn manhattan-perimeter-row
  "Returns the perimeter points on target-row for the point with the given radius"
  [[x y] radius target-row]
  (if (< radius (manhattan-distance [x y] [x target-row]))
    [] ;; outside the radius of this point
    (let [dx (- radius (Math/abs (- y target-row)))]
      [[(+ x (unchecked-negate dx)) target-row]
       [(+ x dx) target-row]])))

;; Sensor at x=2, y=18: closest beacon is at x=-2, y=15
(defn get-input [filename]
  (loop [lines (str/split (slurp filename) #"\n") data []]
    (if (empty? lines)
      data
      (let [line (first lines)
            [_ sx sy bx by] (re-matches #"^Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)" line)
            [sx sy bx by] (map #(Integer/parseInt %) [sx sy bx by])
            distance (manhattan-distance [sx sy] [bx by])
            d {:sensor [sx sy]
               :beacon [bx by]
               :radius distance}]
        (recur (rest lines) (conj data d))))))

(defn solve1 [input target-row]
  (let [beacon-xs (set (distinct (for [entry input
                                       :let [[x y] (:beacon entry)]
                                       :when (= target-row y)]
                                   x)))
        row-vals (for [entry input
                       :let [;; x values in row, either (), (x), or (x1 x2)
                             perimeter (manhattan-perimeter-row
                                        (:sensor entry)
                                        (:radius entry)
                                        target-row)
                             xs (map first (filter (fn [[x y]]
                                                     (= y target-row)) perimeter))
                             xs (case (count xs)
                                  0 nil
                                  1 (first xs)
                                  2 (range (first xs) (inc (second xs))))]]
                   xs)
        row-vals (set (distinct (filter some? (flatten (distinct row-vals)))))
        ;; Remove any known beacons on this row
        row-vals (set/difference row-vals beacon-xs)]
    (count row-vals)
    ))

(defn manhattan-perimeter-range
  "Return a map of point ranges for the given point and radius, at the target row."
  [[x y] radius target-row]
  (let [y-offset (Math/abs (- y target-row))
        rem (- radius (Math/abs y-offset))]
    (if (neg? rem)
      [] ; outside of range
      (let [dx1 (+ x rem)
            dx2 (- x rem)
            tmp (vec (sort [dx1 dx2]))]
        tmp))))

(defn reduce-ranges
  "Given a collection of ranges or single points, reduces them into contiguous ranges where possible.
  Example: ([1 2] 3 [4 6] [9 10]) -> ([1 6] [9 10])"
  [ranges]
  (let [sorted-ranges (sort ranges)]
    (loop [remaining (rest sorted-ranges)
           curr (first sorted-ranges)
           res []]
      (if (empty? remaining)
        (conj res curr)
        (let [[from to] (first remaining)
              [currfrom currto] curr
              newcurr (if (<= (dec from) currto)
                        [(min from currfrom) (max to currto)] ; overlap
                        [from to])
              newres (if (<= (dec from) currto)
                       res ; overlap was merged into curr
                       (conj res curr))]
          (recur (rest remaining) newcurr newres))))))

(defn restrict-ranges [min-val max-val ranges]
  (loop [ranges ranges res []]
    (if (empty? ranges)
      res
      (let [[a b] (first ranges)
            newres (if (or (< b min-val)
                           (> a max-val))
                     res ; drop it
                     (conj res [(if (< a min-val) min-val a)
                                (if (> b max-val) max-val b)]))]
        (recur (rest ranges) newres)))))

(defn find-gaps [ranges]
  (loop [remaining (rest ranges)
         curr (first ranges)
         res []]
    (if (empty? remaining)
      res
      (let [l (second curr)
            r (ffirst remaining)]
        (range (inc l) r)))))

(defn solve2 [input max-x max-y]
  (loop [y max-y]
    (if (neg? y)
      nil
      (let [ranges (filter some? (map not-empty
                                      (for [entry input]
                                        (manhattan-perimeter-range (:sensor entry)
                                                                   (:radius entry)
                                                                   y))))
            reduced-ranges (reduce-ranges ranges)
            restricted-ranges (restrict-ranges 0 max-x reduced-ranges)
            gaps (find-gaps restricted-ranges)]
        (if (empty? gaps)
          (recur (dec y))
          (+ y (* (first gaps) 4000000)))))))

(defn solve []
  (let [
        filename "inputs/2022/day15.txt" target-row 2000000
        max-x 4000000
        max-y 4000000
        input (get-input filename)
        part1 (solve1 input target-row)
        part2 (solve2 input max-x max-y)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
