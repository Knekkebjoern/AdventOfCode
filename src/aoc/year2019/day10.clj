(ns aoc.year2019.day10
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-map [filename]
  (let [lines (str/split (slurp filename) #"\n")
        map-data  (reduce merge
                          (for [y (range (count lines))
                                :let [l (get lines y)]]
                            (reduce merge
                                    (for [x (range (count l))
                                          :let [c (get l x)]]
                                      {[x y] c}))))]
     map-data))

(defn can-detect [asteroid asteroids]
  (let [[ax ay] asteroid
        asteroids-set (set asteroids)
        get-deltas (fn [a b]
                     [(apply - (map first [a b]))
                      (apply - (map second [a b]))])]
    (for [target asteroids
          :let [[tx ty] target
                [xd yd] (get-deltas target asteroid)
                slope (if (zero? yd) 0 (/ xd yd))
                xstep (if (< xd 0) -1 1)
                ystep (if (< yd 0) -1 1)
                xrange (if (zero? xd)
                         [ax]
                         (range (+ ax xstep) tx xstep))
                yrange (if (zero? yd)
                         [ay]
                         (range (+ ay ystep) ty ystep))
                locs (filter #(not (= [0 0] %))
                             (for [x xrange
                                   y yrange]
                               [x y]))
                ;; Potential locations along the line between
                ;; asteroid and target
                locs-on-line (filter #(let [[xd yd] (get-deltas target %)
                                            s (if (zero? yd) 0 (/ xd yd))]
                                        (= slope s))
                                     locs)
                blocks (set/intersection asteroids-set (set locs-on-line))]]
      {:target target :can-detect (empty? blocks) :blocked-by blocks })))

(defn solve1 []
  (let [asteroid-map (get-map "inputs/2019/day10.txt")
        asteroids (map first (filter #(= \# (second %)) asteroid-map))
        detects (reduce merge
                        (for [asteroid asteroids
                              :let [detects (can-detect
                                             asteroid
                                        ; Don't include the asteroid we are checking from
                                             (filter #(not (= asteroid %)) asteroids))
                                    detect-count (count (filter #(:can-detect %) detects))]]
                          {asteroid {:detects detects :detect-count detect-count}}))
        sorted (reverse (sort-by #(get-in (val %) [:detect-count]) detects))]
    (take 1 sorted)))

(defn shoot-asteroid [station angles angle-map res]
  (if (empty? angle-map)
    res
    (let [angle (first angles)
          asteroids (set (get angle-map angle))
          distances (for [asteroid asteroids
                          :let [dx (- (first station) (first asteroid))
                                dy (- (second station) (second asteroid))
                                distance (Math/sqrt (+ (* dx dx) (* dy dy)))]]
                      [distance asteroid])
          nearest-asteroid (second (first (sort-by first < distances)))
          remaining-asteroids (disj asteroids nearest-asteroid)
          new-angle-map (if (empty? remaining-asteroids)
                          (dissoc angle-map angle)
                          (assoc angle-map angle remaining-asteroids))
          new-res (conj res nearest-asteroid)]
      (recur station
             (if (empty? (rest angles)) (sort < (keys new-angle-map)) (rest angles))
             new-angle-map
             new-res))))

(defn solve2 []
  (let [station [26 29]
        asteroid-map (get-map "inputs/2019/day10.txt")
        asteroids (map first (filter #(= \# (second %)) asteroid-map))
        angle-list (for [asteroid asteroids
                         :let [dx (- (first station) (first asteroid))
                               dy (- (second station) (second asteroid))
                               angle (- (Math/toDegrees (Math/atan2 dy dx)) 90)
                               angle (if (< angle 0) (+ angle 360) angle)]]
                     {angle [asteroid]})
        angle-map (apply (partial merge-with into) angle-list)
        order (shoot-asteroid station (sort < (keys angle-map)) angle-map [])
        res (for [x (range (count order))]
              [(inc x) (nth order x)])]
    res))

(defn solve []
  {:part1 (solve1) :part2 nil ;(solve2)
   })

;; Takes too long
(defn status [] "!")
