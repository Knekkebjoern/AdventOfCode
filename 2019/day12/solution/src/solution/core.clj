(ns solution.core
  (:gen-class))
(require '[clojure.math.numeric-tower :as math])


(def io-pos {:x 1 :y 2 :z -9})
(def europa-pos {:x -1 :y -9 :z -4})
(def ganymede-pos {:x 17 :y 6 :z 8})
(def callisto-pos {:x 12 :y 4 :z 2})

(def pairs [[:io :europa] [:io :ganymede]
            [:io :callisto] [:europa :ganymede]
            [:europa :callisto] [:ganymede :callisto]])

(def positions {:io io-pos
                :europa europa-pos
                :ganymede ganymede-pos
                :callisto callisto-pos})

(def velocities {:io {:x 0 :y 0 :z 0}
                 :europa {:x 0 :y 0 :z 0}
                 :ganymede {:x 0 :y 0 :z 0}
                 :callisto {:x 0 :y 0 :z 0}})

(defn calculate1 [steps positions velocities]
  (if (zero? steps)
    {:positions positions :velocities velocities}
    (let [data (for [[b a] pairs
                     d [:x :y :z]
                     :let [av (d (a positions))
                           bv (d (b positions))]]
                 (if (< av bv)
                   {a {d 1} b {d -1}}
                   (if (> av bv)
                     {a {d -1} b {d 1}}
                     {})))
          gravity (apply merge-with (partial merge-with +) data)
          ;; a (println steps  "=================")
          ;; a (println "positions" (vals positions))
          ;; a (println "velocities" (vals velocities))
          ;; a (println "gravity" (vals gravity))
          new-velocities (merge-with (partial merge-with +) velocities gravity)
          new-positions (merge-with (partial merge-with +) positions new-velocities)
          ;; a (println "new-positions" (vals new-positions))
          ;;a (println "new-velocities" (vals new-velocities))
          ]
      (recur (dec steps) new-positions new-velocities))))


(defn solution1
  "I don't do a whole lot ... yet."
  [& args]
  (let [step-count 1000
        res (calculate step-count positions velocities)

        pot (apply merge
                   (for [moon (keys (:positions res))]
                     {moon (reduce + (map math/abs (vals (moon (:positions res)))))}))
        kin (apply merge
                   (for [moon (keys (:velocities res))]
                     {moon (reduce + (map math/abs (vals (moon (:velocities res)))))}))
        a (println "potential" pot)
        a (println "kinetic" kin)
        total (merge-with * pot kin)
        a (println "total" total)
        total (reduce + (vals total))
        a (println "total" total)
        ]
    ))

(defn get-key [positions velocities]
  (clojure.string/join  "," (for [m [:io :europa :ganymede :callisto]
                                  l [:x :y :z]]
                              (str (l (m positions))
                                   (l (m velocities))))))

(defn calculate2 [positions velocities target step]
  (let [new-key (get-key positions velocities)
        ;a (println new-key)
        a (if (zero? (rem step 100000)) (println "Steps: " step))]
    (if (and (not (zero? step))
             (= target new-key))
      (do
        (println new-key)
        step)
      (let [data (for [[b a] pairs
                       d [:x :y :z]
                       :let [av (d (a positions))
                             bv (d (b positions))]]
                   (if (< av bv)
                     {a {d 1} b {d -1}}
                     (if (> av bv)
                       {a {d -1} b {d 1}}
                       {})))
            gravity (apply merge-with (partial merge-with +) data)
            new-velocities (merge-with (partial merge-with +) velocities gravity)
            new-positions (merge-with (partial merge-with +) positions new-velocities)
            ]
        (recur new-positions new-velocities target (inc step))))))


(defn solution2
  ""
  [& args]
  (let [res (calculate2 positions velocities (get-key positions velocities) 0)]
            (println res)
))
