(ns aoc.year2024.day5
  (:require
   [aoc.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]))

(comment
  (aoc.core/fetch-input! 2024 5))

(def input-filename "inputs/2024/day5.txt")
;;(def input-filename "inputs/2024/day5-test.txt")

(defn get-input []
  (let [data (io/read-file input-filename)
        orders (for [[_ _ a b] (re-seq #"((\d+)\|(\d+))" data)]
                 [a b])
        orders (group-by (fn [[a b]] a) orders)
        orders (into {} (for [[k vs] orders]
                          {(parse-long k) (set (map parse-long (map second vs)))}))
        publications (doall
                      (for [p (filter #(re-find #"," %1)  (io/split-lines data))]
                        (map parse-long (str/split p #","))))]
    {:orders orders
     :publications publications}))

(defn get-middle [pub]
  (nth pub (int (/ (count pub) 2))))

(defn valid-publication? [pub orders]
  ;;(println "\npub" pub "orders" orders)
  (loop [pub pub
         seen #{}
         res true]
    (if (or (not res)
            (empty? pub))
      res
      (let [n (first pub)
            rule (get orders n #{})
            res  (empty? (set/intersection seen rule)) ;; we haven't seen any pages that should come later
            seen (conj seen n)]
        (recur (rest pub) seen res)))))

(defn solve1 [input]
  (let [{:keys [orders publications]} input]
    (loop [publications publications
           res []]
      (if (empty? publications)
        (apply + res)
        (let [pub (first publications)
              res (if (valid-publication? pub orders)
                    (conj res (get-middle pub))
                    res)]
          (recur (rest publications) res))))))

(defn fix-publication [pub orders]
  (let [comparator (fn [a b]
                     (cond
                       (= a b) 0
                       (valid-publication? [a b] orders) -1
                       (valid-publication? [b a] orders) 1
                       :else (throw (Exception. (str "Unknown ordering for " [a b] " given " orders)))))]
    (sort comparator pub)))

(defn solve2 [input]
  (let [{:keys [orders publications]} input]
    (loop [publications publications
           res []]
      (if (empty? publications)
        (apply + res)
        (let [pub (first publications)
              res (if (valid-publication? pub orders)
                    res
                    (conj res (get-middle (fix-publication pub orders))))]
          (recur (rest publications) res))))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))
(defn status [] "*")
