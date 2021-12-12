(ns year_2021.day11)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data  (loop [lines lines y 0 res {}]
                (if (empty? lines)
                  res                  
                  (let [line (first lines)
                        xs (map #(Integer/parseInt (str %)) line)
                        d (reduce-kv (fn [res idx itm ]
                                       (assoc res [idx y] itm)) {} (into [] xs))]
                    (recur (rest lines) (inc y) (merge res d)))))]
    data))


(defn get-mask [x y d]
  (for [xx (range (- x d) (+ x d 1))
        yy (range (- y d) (+ y d 1))
        :when (and (<= 0 xx)
                   (<= 0 yy)
                   (not (= [xx yy] [x y])))]
    [xx yy]))

(defn get-neighbors [[x y] data]
  (keys (select-keys data (get-mask x y 1))))

(defn increase-all [data]
  (reduce-kv (fn [res k v] (assoc res k (inc v))) {} data))

(defn update-some [f data idxs]
  (loop [data data idxs idxs]
    (if (empty? idxs)
      data
      (recur (assoc data (first idxs) (f (get data (first idxs)))) (rest idxs)))))


(defn step [input]
  (let [data (:data input)
        flashes (:flashes input)
        s1 (increase-all data)
        s2 (loop [population s1 flashed #{}]
             (let [can-flash (into #{}
                                   (keys (filter (fn [[k v]] (> v 9)) population)))
                   should-flash (clojure.set/difference can-flash flashed)]
               (if (empty? should-flash)
                 {:data (update-some (fn [_] 0) population flashed) 
                  :flashes (+ flashes (count flashed))} 
                 (let [get-flashed (apply concat (map #(get-neighbors % population) should-flash))
                       ;;a (prn "get-flashed" get-flashed)
                       increased (update-some inc population get-flashed)
                       did-flash (set (concat should-flash flashed))]
                   (recur increased did-flash)))))]    
    s2))

(defn show [data]
  (let [vs (partition 10 (for [y (range 0 10) x (range 0 10)] (get data [x y])))]
    (doseq [l vs] (prn l))))


(defn solve1 [data]
  (loop [data data i 100]
    (if (zero? i)
      data
      (recur (step data) (dec i)))))

(defn all-blink [data]
  (= 1 (count (keys (frequencies (flatten (vals data)))))))

(defn solve2 [input]
  (loop [input input n 1]
    (let [s1 (step input)]
      (if (all-blink (:data s1))
        {:data s1 :n n}
        (recur s1 (inc n))))))

(let [input1 (get-input"inputs/day11_input1.txt")      
      part1 (solve1 {:data input1 :flashes 0})
      part2 (solve2 {:data input1 :flashes 0})]
  (println "Part1: " part1)
  (println "Part2: " part2))

