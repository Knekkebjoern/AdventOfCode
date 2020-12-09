(ns day8.core
  (:gen-class))


(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-input [filename]
  (concat
   (for [line (str/split (slurp filename) #"\n")
         :let [[op arg1] (str/split line #"\s+")]]
     {:op op
      :arg1 (Integer/parseInt arg1)
      :callcount 0})
   [{:op nil}])) ; nil terminated list of ops

(defn jump [n ops]
  (let [[left right] (split-at (if (pos? n) n (+ (count ops) n)) ops)]
    (concat right left)))

(defn solve [ops accumulator]
  (loop [ops ops accumulator accumulator]
    (let [op (first ops)]
      (if (nil? (:op op))
        [true accumulator] ; success
        (if (<= 1 (:callcount op))
          [false accumulator] ; infinite loop
          (let [newop (update-in op [:callcount] inc)
                newops (concat [newop] (rest ops))]
            (case (:op newop)
              "acc" (recur (jump 1 newops)
                           (+ accumulator (:arg1 newop)))
              "nop" (recur (jump 1 newops) accumulator)
              "jmp" (recur (jump (:arg1 newop) newops) accumulator ))))))))

(defn solve2 [left right]
  (loop [left left right right]
    (if (empty? right)
      nil
      (let [op (update-in (first right) [:op] #(case %
                                                 "nop" "jmp"
                                                 "jmp" "nop"
                                                 %
                                                 ))
            newops (concat left (concat [op] (rest right)))
            [success accumulator] (solve newops 0)]
        (if success
          accumulator
          (recur (conj left (first right)) (rest right)))))
    ))

(defn -main
  [& args]
  (let [input (get-input "input.txt")
        part1 (solve input 0)
        part2 (solve2 [] input)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))
