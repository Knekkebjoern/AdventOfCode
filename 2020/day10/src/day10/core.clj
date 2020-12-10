(ns day10.core
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (for [line (str/split (slurp filename) #"\n")]
      (read-string line)))

(defn solve1 [input]
  (frequencies (for [pair (partition 2 1 (sort input))]
                (apply - pair))))

;; (defn solve2 [input]
;;   (if (empty? input)
;;     []
;;     (if (= 1 (count input))
;;       input
;;       (let [high (+ 3 (first input))
;;             a (prn "Input: " input)
;;             a (prn "High: " high)
;;             ]

;;         (loop [input (rest input) res []]
;;           (if (empty? input)
;;             res
;;             (if (< high (first input))
;;               res
;;               (let [a (prn "-------")
;;                     a (prn "Input: " input)
;;                     a (prn "Res:" res)
;;                     newres (conj res (solve2 (rest input)))
;;                     a (prn "Newres:" newres)]
;;                 (recur (rest input) newres)))))))
;;       ))

(def input (sort (get-input "input3.txt")))

;(solve2 (sort input))



;(def input [1 3 4 6])

(loop [input (reverse (sort input)) cache {} res []]
  (if (empty? input)
    res
    (let [
          a (prn "---------")
          a (prn "input" input)
          a (prn "cache" cache)
          target (first input)
          compatible (take-while #(<= (- target %) 3) (rest input))
          cache (merge-with into cache (into {} (for [x compatible]
                                                  {x ])})))
          a (prn "target" target)
          a (prn "compatible" compatible)
          a (prn "cache" cache)
]
      (recur (rest input) cache res))))

;; (let [input (get-input "input2.txt")
;;       stage1 (solve1 input)]
;;   (println "Stage1:" (* (inc (get stage1 -1)) (inc (get stage1 -3)))))
