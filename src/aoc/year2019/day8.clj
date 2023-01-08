(ns aoc.year2019.day8
  (:gen-class))
(require '[clojure.string :as str])


(defn get-layers [filename layer-size]
  (let [data (first (str/split (slurp filename) #"\n"))
        layers (partition layer-size data)]
    layers))


(defn solve []
  (let [layers (get-layers "inputs/2019/day8.txt" (* 25 6))
        freqs (for [x layers
                    :let [f (frequencies x)]]
                {:freqs f :sort-by (get f \0)})
        layer (first (sort-by :sort-by freqs))
        d (layer :freqs)
        pixel-data (apply map vector layers)
        pixels (for [x pixel-data]
                 (first (drop-while #(= %1 \2) x)))
        pixels (for [x pixels] (if (= x \1) \. \M))]
    (let [res1 (* (get d \1) (get d \2))
          res2 (dorun (for  [x (partition 25 pixels )]
                        (println x)))]
      {:part1 res1 :part2 res2})))

(defn status [] "*")
