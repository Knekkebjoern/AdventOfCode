(ns aoc.io
  (:require [clojure.string :as str]))

(defn read-file [filename]
  (slurp filename))

(defn split-lines [s]
  (str/split-lines s))

;; (parse-lines-as-grid ["abcdefg" "hijklmn"])
;; => {[0 0] \a, [1 0] \b, [1 1] \i, [3 0] \d, [4 1] \l, [5 1] \m, [6 1] \n, [2 0] \c, [3 1] \k, [2 1] \j, [5 0] \f, [6 0] \g, [0 1] \h, [4 0] \e}
(defn parse-lines-as-grid
  "Reads lines and returns a map of coordinate `[x y]` keys and `char` values, with [0, 0] at the upper left corner, with `x` increasing to the right and `y` increasing down."
  [lines]
  (loop [lines lines
         y 0
         res {}]
    (if (empty? lines)
      res
      (let [tmp (apply merge (map-indexed (fn [i c] {[i, y] c}) (first lines)))
            res (merge res tmp)]
        (recur (rest lines) (inc y) res)))))
