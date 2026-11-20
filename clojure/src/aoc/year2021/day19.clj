(ns aoc.year2021.day19)
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.math.numeric-tower :as math])
(use 'clojure.set)

(set! *warn-on-reflection* true)

(defn get-input [filename]
  (let [all-lines (filter #(not (empty? %)) (str/split (slurp filename) #"\n"))
        data  (loop [lines all-lines scanner-id nil res {}]
                (if (empty? lines)
                  res
                  (let [line (first lines)
                        [_ new-id] (re-find #"--- scanner (\d+) ---" line)]
                    (if (some? new-id)
                      (recur (rest lines)
                             (Integer/parseInt new-id)
                             (assoc res (Integer/parseInt new-id) []))
                      (let [coords (map #(Integer/parseInt %) (str/split line #","))]
                        (recur (rest lines) scanner-id
                               (update res scanner-id (fn [old]
                                                        (conj old coords)))))))
                  ))]
    data))

(def all-rotations [[:x :y :z] [:x :-z :y] [:x :-y :-z] [:x :z :-y]
                    [:-x :-y :z] [:-x :-z :-y] [:-x :y :-z] [:-x :z :y]
                    [:y :x :-z] [:y :-x :z] [:y :z :x] [:y :-z :-x]
                    [:-y :x :z] [:-y :-x :-z] [:-y :-z :x] [:-y :z :-x]
                    [:z :x :y] [:z :-x :-y] [:z :-y :x] [:z :y :-x]
                    [:-z :x :-y] [:-z :-x :y] [:-z :y :x] [:-z :-y :-x]])

(defn rotate [[x y z] rotation]
  (into []
        (for [k rotation]
          (case k
            :x x
            :y y
            :z z
            :-x (* -1 x)
            :-y (* -1 y)
            :-z (* -1 z)))))

(defn get-signatures [coords]
  (apply merge
         (for [b1 coords b2 coords
               :when (not= b1 b2)
               :let [sig1 (into [] (map - b1 b2))
                     ;;sig2 (into [] (map - b2 b1))
                     ]]
           {sig1 {:from b1 :to b2}
            ;;sig2 {:from b2 :to b1}
            })))

(defn find-mappings [base-case-id base-case-signatures scanner-signatures data]
  (reduce-kv
   (fn [res k v]
     (if (empty? v)
       res
       (assoc res k v)))
   {}
   (apply merge
          (let [signatures-1 (set (keys base-case-signatures))]
            (for [signature-2 (keys scanner-signatures)
                  :let [signatures-2 (set (keys (get scanner-signatures signature-2)))
                        inter (clojure.set/intersection signatures-1 signatures-2)
                        sample-signature (first inter)
                        base-sample-coord (:from (get base-case-signatures sample-signature))
                        scanner-sample-coord (:from (get (get scanner-signatures signature-2) sample-signature))
                        translation (map - base-sample-coord scanner-sample-coord)
                        [scanner-2-id rotation-2] signature-2
                        translated-coords (for [coord (get data scanner-2-id)]
                                            ;; rotate then translate
                                            (map + translation (rotate coord rotation-2)))]
                  :when (and (not= base-case-id signature-2)
                             (>= (count inter) 12))]
              {[base-case-id signature-2] {:translation translation
                                           :base (get base-case-signatures sample-signature)
                                           :scanner (get (get scanner-signatures signature-2) sample-signature)
                                           :sample sample-signature
                                           :translated-coords translated-coords
                                           :scanner-2-id scanner-2-id
                                           }})))))


(defn get-scanner-signatures [scanner-id scanner-data rotations]
  (apply merge
         (for [rotation rotations
               :let [rotated-data (map #(rotate % rotation)
                                       scanner-data)]]
           {[scanner-id rotation] (get-signatures rotated-data)})))

(def data (get-input "inputs/day19.txt"))

(def mappings
  (let [base-case-rotation [:x :y :z]]
    (apply concat
           (for [base-case-id (keys data)]
             (let [base-case-key [base-case-id base-case-rotation]
                   base-case-signatures (get-scanner-signatures
                                         base-case-id
                                         (get data base-case-id)
                                         [base-case-rotation])
                   base-case-signatures (get base-case-signatures base-case-key)]
               (apply concat
                      (for [scanner-id (keys data)
                            :when (not= scanner-id base-case-id)
                            :let [scanner-signatures (get-scanner-signatures
                                                      scanner-id
                                                      (get data scanner-id)
                                                      all-rotations)
                                  mappings (find-mappings base-case-key
                                                         base-case-signatures
                                                         scanner-signatures
                                                         data)]]
                        mappings)))))))

(defn transform [rotation translation coords]
  (for [coord coords]
    ;; rotate then translate
    (map + translation (rotate coord rotation)))  )


(def mappings-by-ids (apply merge
                            (for [[k v] mappings
                                  :let [[[a-id _] [b-id b-rot]] k
                                        translation (:translation v)]]
                              {[a-id b-id] {:rotation b-rot :translation translation}})))

(def todo [0 25
           0 10
           25 23
           23 20
           23 13
           23 17
           20 7
           20 27
           20 1
           20 4
           20 12
           20 2
           20 19
           13 29
           13 5
           7 15
           7 6
           7 11
           7 9
           7 26
           27 3
           4 21
           12 24
           15 22
           15 16
           6 28
           9 18
           22 14
           18 8])

(def output (let [mappings (for [[a b] (reverse (partition 2 todo))] [a b])]
              (loop [mappings mappings res data]
                (if (empty? mappings)
                  res
                  (let [mapping (first mappings)
                        {rotation :rotation translation :translation} (get mappings-by-ids mapping)
                        [a-id b-id] mapping
                        new-coords (set (transform rotation translation (get res b-id)))
                        new-res (merge-with union {a-id new-coords} res)]
                    (recur (rest mappings) new-res))))))

;; Part 1
(count (get output 0))

;; Part 2 - same as part 1, but only translate the scanner locations to the
;; scanner 0 reference coords
(def data2 (apply (partial merge-with union)
                       (for [[[a-id b-id] {translation :translation}] mappings-by-ids]
                         {a-id (set [translation])})))

(def output2 (let [mappings (for [[a b] (reverse (partition 2 todo))] [a b])]
               (loop [mappings mappings res data2]
                 (if (empty? mappings)
                   res
                   (let [mapping (first mappings)
                         {rotation :rotation translation :translation} (get mappings-by-ids mapping)
                         [a-id b-id] mapping
                         new-coords (set (transform rotation translation (get res b-id)))
                         new-res (merge-with union {a-id new-coords} res)]
                     (recur (rest mappings) new-res))))))

(def tmp (for [a (get output2 0)
               b (get output2 0)]
           (reduce + (map math/abs (map - a b)))))
(apply max tmp)


;; Needs cleanup
(defn status [] "!")
