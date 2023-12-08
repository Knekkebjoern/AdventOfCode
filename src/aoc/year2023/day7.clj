(ns aoc.year2023.day7
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]))

(comment
  (aoc.core/fetch-input! 2023 7)
  )

;;(def input-filename "inputs/2023/day7-test.txt")
(def input-filename "inputs/2023/day7.txt")


(defn upgrade-hand [hand]
  (let [t (get-hand-type hand)
        freq (frequencies hand)]
    (if (nil? (get freq :J))
      hand
      (let [pref (into (sorted-map-by (fn [key1 key2]
                                        (compare (key2 freq)
                                                 (key1 freq)))) freq)
            candidate (loop [freq freq [cur cur-cnt] [:2 1]]
                        (if (empty? freq)
                          cur
                          (if (= :J (first (first freq))) ; skip
                            (recur (rest freq) [cur cur-cnt])
                            (let [[x x-cnt] (first freq)
                                  [x-val cur-val] (map #(get card-vals %) [x cur])
                                  new-res (cond
                                            (> x-cnt cur-cnt) :x
                                            (= x-cnt cur-cnt) (if (> x-val cur-val) :x :cur)
                                            :default :cur)
                                  new-res (if (= new-res :cur)
                                            [cur cur-cnt]
                                            [x x-cnt])]
                              (recur (rest freq) new-res)))))
            new-hand (for [x hand]
                       (case x
                         :J candidate
                         x))]
        new-hand))))

(defn get-input []
  (let [lines (map rest (re-seq #"(.*?) (\d+)" (slurp input-filename)))]
    (for [[h b] lines
          :let [hand (map keyword (map str h))
                best-hand (upgrade-hand hand)]]
      {:hand hand :best best-hand :bid (read-string b)})))

(def type-vals {:five-kind 100
                :four-kind 99
                :full-house 98
                :three-kind 97
                :two-pair 96
                :one-pair 95
                :high-card 94})

(def card-vals {:A 14
                :K 13
                :Q 12
                :J 11
                :T 10
                :9 9
                :8 8
                :7 7
                :6 6
                :5 5
                :4 4
                :3 3
                :2 2})

(defn get-hand-type [hand]
  (let [vs (vals (frequencies hand))
        cnts (set vs)
        ;;a (println "counts" cnts)
        ]
    (cond
      (contains? cnts 5) (:five-kind type-vals)
      (contains? cnts 4) (:four-kind type-vals)
      (and (contains? cnts 3)
           (contains? cnts 2)) (:full-house type-vals)
      (contains? cnts 3) (:three-kind type-vals)
      (= 2 (count (filter #(= 2 %) vs))) (:two-pair type-vals)
      (= 1 (count (filter #(= 2 %) vs))) (:one-pair type-vals)
      :default (:high-card type-vals))))

(defn hands-comparator [mode data1 data2]
  (let [k (if (= mode :part1) :hand :best)
        hand1 (get data1 k)
        hand2 (get data2 k)
        type1 (get-hand-type hand1)
        type2 (get-hand-type hand2)
        card-val-f (fn [x]
                     (if (and (= x :J)
                              (= mode :part2))
                       1
                       (get card-vals x)))]
    (if (not= type1 type2)
      (compare type1 type2)
      ;; Always compare base hand if same type
      (loop [hand1 (get data1 :hand)
             hand2 (get data2 :hand)]
        (if (empty? hand1)
          0
          (let [[f1 f2] (map first [hand1 hand2])]
            (if (not= f1 f2)
              (let [v1 (card-val-f f1)
                    v2 (card-val-f f2)
                    d (compare v1 v2)]
                d)
              (recur (rest hand1) (rest hand2)))))))))

(defn solve1 [mode input]
  (let [sorted (sort (partial hands-comparator mode) input)
        bids (map :bid sorted)
        totals (map-indexed #(* (inc %1) %2) bids)]
    (reduce + totals)))


(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 :part1 input)
        part2 (solve1 :part2 input)
        ]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
