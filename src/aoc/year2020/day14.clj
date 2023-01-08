(ns aoc.year2020.day14
  (:require [clojure.string :as str]))

(defn get-input [filename]
  (for [line (str/split (slurp filename) #"\n")
        :let [[_ command arg] (re-matches #"^(.*?) = (.*?)$" line)
              [_ ind] (re-matches #"^mem\[(\d+)\]$" command)]]
    (if ind
      { :op :mem
       :index (BigInteger. ind)
       :indexbin (Integer/toString (BigInteger. ind) 2)
       :arg (BigInteger. arg)
       :argbin (Integer/toString (BigInteger. ind) 2) }
      { :op :mask :arg arg })))

(defn mask-value [mask value]
  (loop [mask (reverse mask) value (reverse value) res ""]
    (if (empty? mask)
      (apply str (reverse res))
      (recur (rest mask) (rest value) (str res (case (first mask)
                                                 \X (or (first value) \0)
                                                 (first mask)))))))

(defn solve1 [commands]
  (loop [commands commands memory {} mask ""]
    (if (empty? commands)
      memory
      (let [command (first commands)
            newmask (if (= (:op command) :mask)
                      (:arg command)
                      mask)
            newmemory (if (= (:op command) :mem)
                        (assoc memory (:index command)
                               (mask-value mask (:argbin command)))
                        memory)]
        (recur (rest commands) newmemory newmask)))))


(defn floating-mask-value [mask value]
  (loop [mask (reverse mask) value (reverse value) res [[]]]
    (if (empty? mask)
      (map #(BigInteger. (apply str (reverse %)) 2) res)
      (let [newbits (case (first mask)
                      \1 [1]
                      \0 [(or (first value) \0)]
                      [0 1])
            newres (for [r res b newbits]
                     (conj r b))]
        (recur (rest mask) (rest value) newres)))))

(defn solve2 [commands]
  (loop [commands commands memory {} mask ""]
    (if (empty? commands)
      memory
      (let [command (first commands)
            newmask (if (= (:op command) :mask)
                      (:arg command)
                      mask)
            newmemory (if (= (:op command) :mem)
                        (let [addresses (floating-mask-value newmask
                                                             (:indexbin command))]
                          (apply merge (for [i addresses] {i (:arg command)})))
                        memory)]
        (recur (rest commands) (merge memory newmemory) newmask)))))

(defn solve
  [& args]
  (let [input (get-input "inputs/2020/day14.txt")
        state1 (solve1 input)
        result1 (reduce + (map #(read-string (str "2r" %)) (vals state1)))
        state2 (solve2 input)
        result2 (reduce + (vals state2))]
    {:part1 result1 :part2 result2}))

; Part1 returns wrong answer somehow
(defn status [] "!")
