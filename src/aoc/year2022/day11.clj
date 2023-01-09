(ns aoc.year2022.day11)
(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn from-operation [xs]
  (for [x xs]
    (cond
      (= x "old") :OLD
      (= x "new") :NEW
      (= x "+") +
      (= x "*") *
      :else (if-let [[_ d] (re-matches #"^(\d+)$" x)]
              (Integer/parseInt d)
              nil))))

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]
    (loop [lines lines res {} cur nil]
      ;(println (first lines))
      (if (empty? lines)
        res
        (let [line (first lines)
              newcur (if-let [[_ x] (re-matches #"^Monkey (\d+):$" line)]
                       (Integer/parseInt x)
                       cur)]
          (if (not= cur newcur)
            (recur (rest lines) res newcur)
            (let [newitems (if-let [[_ x]
                                    (re-matches #"^.*Starting items: (.*?)$" line)]
                             (map #(Integer/parseInt %) (str/split x #"\s*,\s*"))
                             (get-in res [newcur :items] []))
                  newoper (if-let [[_ a oper b]
                                   (re-matches #"^.*Operation: new = (.*?) (.*?) (.*?)$" line)]
                            (from-operation [oper a b])
                            (get-in res [newcur :operation] nil))
                  newtest (if-let [[_ d] (re-matches #"^.*Test: divisible by (\d+)$" line)]
                            (Integer/parseInt d)
                            (get-in res [newcur :test] nil))
                  newaction (if-let [[_ x d] (re-matches #"^.*If (true|false): throw to monkey (\d+)$" line)]
                              (assoc  (get-in res [newcur :action] {})
                                      (keyword x) (Integer/parseInt d))
                              (get-in res [newcur :action] {})
                              )
                  ]
              (recur (rest lines)
                     (assoc res newcur {:id newcur
                                        :items (vec newitems)
                                        :operation newoper
                                        :test newtest
                                        :action newaction
                                        :inspected 0})
                     newcur))))))))


(defn solve1 [data relief rounds]
  (loop [data data cur 0 round 1]
    (let [items (get-in data [cur :items] [])]
      (if (empty? items)
        (if (get data (inc cur))
          (recur data (inc cur) round) ;; go on to next monkey
          (if (< round rounds)
            (do
              (if (zero? (mod round 1000))
                (println round (for [[_ v] data]
                                 (:inspected v))))
              (recur data 0 (inc round))) ;; go on to next round
            (reduce * (take 2 (reverse (sort (for [[_ v] data]
                                               (:inspected v)))))))) ;; all done
        (let [item (first items)
              operation (get-in data [cur :operation])
              newworry (for [x operation]
                         (if (= :OLD x) item x))
              newworry (eval newworry)
              newworry (Math/floor(mod newworry relief))
              action (keyword (str (zero? (mod newworry (get-in data [cur :test])))))
              throwto (get-in data [cur :action action])
              newdata (-> data
                          (update-in [cur :inspected] inc)
                          (assoc-in [cur :items] (vec (rest items)))
                          (assoc-in [throwto :items] (conj (get-in data [throwto :items]) newworry)))]
          (recur newdata cur round))))))

(defn solve []
  (let [input (get-input "inputs/2022/day11.txt")
        part1 (solve1 input 3 20)
        relief (reduce * (for [[_ v] input] (:test v)))
        part2 (solve1 input relief 10000)]
    {:part1 part1 :part2 part2}))


;; too slow
(defn status [] "!")
