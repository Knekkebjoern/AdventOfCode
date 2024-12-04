(ns aoc.year2024.day4
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as cset]
            [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 4))

;;(def input-filename "inputs/2024/day4-test.txt")
(def input-filename "inputs/2024/day4.txt")

(defn get-input []
  (let [lines (io/split-lines (io/read-file input-filename))]
    (loop [lines lines
           y 0
           res {}]
      (if (empty? lines)
        res
        (let [tmp (apply merge (map-indexed (fn [i c] {[i, y] c}) (first lines)))
              res (merge res tmp)]
          (recur (rest lines) (inc y) res))))))

;;   0123456789
;; 0 ....XXMAS.
;; 1 .SAMXMS...
;; 2 ...S..A...
;; 3 ..A.A.MS.X
;; 4 XMASAMX.MM
;; 5 X.....XA.A
;; 6 S.S.S.S.SS
;; 7 .A.A.A.A.A
;; 8 ..M.M.M.MM
;; 9 .X.X.XMASX

(defn xmas-count [data [x y]]
  (if (not (= \X (get data [x y])))
    0
    (let [s (fn [[dx dy] c]
              (= c (get data [(+ x dx) (+ y dy)])))
          tmp [(and (s [1 0] \M)        ; left-to-right
                    (s [2 0] \A)
                    (s [3 0] \S))
               (and (s [-1 0] \M)       ; right-to-left
                    (s [-2 0] \A)
                    (s [-3 0] \S))
               (and (s [0 -1] \M)       ; upwards
                    (s [0 -2] \A)
                    (s [0 -3] \S))
               (and (s [0 1] \M)        ; downwards
                    (s [0 2] \A)
                    (s [0 3] \S))
               (and (s [1 -1] \M)       ; up-right
                    (s [2 -2] \A)
                    (s [3 -3] \S))
               (and (s [1 1] \M)        ; down-right
                    (s [2 2] \A)
                    (s [3 3] \S))
               (and (s [-1 -1] \M)      ; up-left
                    (s [-2 -2] \A)
                    (s [-3 -3] \S))
               (and (s [-1 1] \M)       ; down-left
                    (s [-2 2] \A)
                    (s [-3 3] \S))]
          ;_ (println [x y] "tmp:" tmp "=>" (count (filter true? tmp)))
          ]
      (count (filter true? tmp)))))

;;   0123456789
;; 0 .M.S......
;; 1 ..A..MSMS.
;; 2 .M.S.MAA..
;; 3 ..A.ASMSM.
;; 4 .M.S.M....
;; 5 ..........
;; 6 S.S.S.S.S.
;; 7 .A.A.A.A..
;; 8 M.M.M.M.M.
;; 9 ..........

(defn x-mas-count [data [x y]]
  (if (not (= \A (get data [x y])))
    0
    (let [s (fn [[dx dy] c]
              (= c (get data [(+ x dx) (+ y dy)])))
          up-right (and (s [-1 1] \M)   ; up-right
                        (s [1 -1] \S))
          down-right (and (s [-1 -1] \M) ; down-right
                          (s [1 1] \S))
          up-left (and (s [1 1] \M)     ; up-left
                       (s [-1 -1] \S))
          down-left (and (s [1 -1] \M)  ; down-left
                         (s [-1 1] \S))

          tmp [(and up-right down-right)
               (and up-right up-left)
               (and down-right down-left)
               (and down-left up-left)]

          _ (println [x y] "tmp:" tmp "=>" (count (filter true? tmp)))]
      (count (filter true? tmp)))))

(defn solve1 [input]
  (let [hits (map (partial xmas-count input) (sort (keys input)))]
    hits))

(defn solve2 [input]
  (let [hits (map (partial x-mas-count input) (sort (keys input)))]
    (println "hits" (apply + hits))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        ;;_ (println input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(solve)
(defn status [] "*")
