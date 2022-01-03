(ns year_2021.day21)
(require '[clojure.string :as str])

(defn dice-f []
  (let [tick (atom 0)]
    (fn []
      (swap! tick #(if (= % 100) 1 (inc %))))))

(def roll-dice (dice-f))

(defn play1 [p1-pos p2-pos]
  (loop [data {:p1 {:pos p1-pos :score 0}
               :p2 {:pos p2-pos :score 0}
               :dice-rolls 0
               :curr :p1}]
    (let [{dice-rolls :dice-rolls
           curr :curr} data
          ;;_ (prn data)
          ]
      (if (or (<= 1000 (:score (:p1 data)))
              (<= 1000 (:score (:p2 data))))
        data
        (let [roll (+ (roll-dice) (roll-dice) (roll-dice))
              new-rolls (+ 3 dice-rolls)
              
              new-pos (loop [pos (+ roll (:pos (get data curr)))]
                        (if (<= pos 10) pos (recur (- pos 10))))
              new-score (+ new-pos (:score (get data curr)))
              new-data (update-in data [curr] assoc :pos new-pos :score new-score)
              new-data (assoc new-data :dice-rolls new-rolls :curr (if (= curr :p1) :p2 :p1))]
          (recur new-data))))))

(defn solve1 []
  (let [res (play1 6 8)
        p1-score (:score (:p1 res))
        p2-score (:score (:p2 res))
        losing-score (min p1-score p2-score)]
    (println "Part1" (* (:dice-rolls res) losing-score))))

;; Part 2

(def freqs [[3 1]
            [4 3]
            [5 6]
            [6 7]
            [7 6]
            [8 3]
            [9 1]])

(defn update-turn-data [roll data]
  (if (zero? roll)
    data
    (let [{curr :curr} data
          new-pos (if (zero? roll)
                    (:pos (get data curr))
                    (loop [pos (+ roll (:pos (get data curr)))]
                      (if (<= pos 10) pos (recur (- pos 10)))))
          new-score (if (zero? roll)
                      (:score (get data curr))
                      (+ new-pos (:score (get data curr))))
          new-curr (if (zero? roll)
                     curr
                     (if (= curr :p1) :p2 :p1))
          new-data (update-in data [curr] assoc :pos new-pos :score new-score)
          new-data (assoc new-data :curr new-curr)]
      new-data)))

(defn play-turn [roll data]     
  (let [new-data (update-turn-data roll data)
        p1-score (:score (:p1 new-data))
        p2-score (:score (:p2 new-data))]
    (if (or (<= 21 p1-score)
            (<= 21 p2-score))
      (if (> p1-score p2-score)
        {:p1 1}
        {:p2 1})
      (let [winners (for [[roll realities] freqs]
                      (apply merge
                             (for [[k v] (play-turn roll new-data)]
                               {k (* v realities)})))
            ret (apply (partial merge-with +) winners)]
        ret))))

(defn solve2 []
  (let [data {:p1 {:pos 6 :score 0}
              :p2 {:pos 8 :score 0}
              :curr :p1}
        res (play-turn 0 data)]
    (println "Part2" res)))

