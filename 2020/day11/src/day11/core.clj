(ns day11.core
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (apply merge (flatten
                (map-indexed (fn [y line]
                               (map-indexed (fn [x c]
                                              {[x y] c}) line))
                             (str/split (slurp filename) #"\n")))))

(defn get-neighbors [_ [x y]]
  (for [xx (range (dec x) (+ 2 x))
        yy (range (dec y) (+ 2 y))
        :when (and (<= 0 xx)
                   (<= 0 yy)
                   (not (= [x y] [xx yy])))
        ]
    [xx yy]))

(def sight-steps (for [x [-1 0 1]
                       y [-1 0 1]
                       :when (not (= [x y] [0 0]))]
                   [x y]))

(defn find-in-sight [room seat direction]
  (loop [pos (map + seat direction)]
    (let [other (get room pos)]
      (if (or (nil? other)
              (not (= other \.)))
        pos
        (recur (map + pos direction))))))

(defn get-visible [room seat]
  (for [direction sight-steps]
    (find-in-sight room seat direction)))

(defn update-seats [room visibility-fn tolerance]
  (loop [room room]
    (let [updated (into {}
                     (for [seat room
                           :let [[ind c] seat
                                 visible (frequencies
                                          (map #(get room %) (visibility-fn room ind)))
                                 occupied (get visible \# 0)
                                 updated-seat (cond
                                                (and (= c \L)
                                                     (zero? occupied))
                                                \#

                                                (and (= c \#)
                                                     (<= tolerance occupied))
                                                \L

                                                :else c)]]
                       [ind updated-seat]))]
      (if (= room updated)
        updated
        (recur updated)))))

(defn -main
  [& args]
  (let [input (get-input "input.txt")]
    (println "Part1: " (frequencies (vals (update-seats input get-neighbors 4))))
    (println "Part2: " (frequencies (vals (update-seats input get-visible 5))))))
