(ns result.core
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
                                      {[x y] c}))))
        ]
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
                ;; a (println "xrange" xrange)
                ;; a (println "yrange" yrange)
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
                ;; a (println "=========")
                ;; a (println "asteroid" asteroid)
                ;; a (println "target" target)
                ;; a (println "slope" slope)
                ;; a (println "locs" locs)
                ;; a (println "locs-on-line" locs-on-line)
                ;; a (println "")
                blocks (set/intersection asteroids-set (set locs-on-line))
                ]
          ]
      {:target target :can-detect (empty? blocks) :blocked-by blocks })))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [asteroid-map (get-map "input.txt")
        asteroids (map first (filter #(= \# (second %)) asteroid-map))
        detects (reduce merge
                        (for [asteroid asteroids
                              :let [detects (can-detect
                                             asteroid
                                        ; Don't include the asteroid we are checking from
                                             (filter #(not (= asteroid %)) asteroids))
                                    detect-count (count (filter #(:can-detect %) detects))]]
                          {asteroid {:detects detects :detect-count detect-count}}))
        sorted (reverse (sort-by #(get-in (val %) [:detect-count]) detects))
        ]
    (take 1 sorted)


    ))
