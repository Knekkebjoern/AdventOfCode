(ns aoc.year2020.day18
  (:require [clojure.string :as str]))

(defn next-token [s]
  (loop [s s res []]
    (if (empty? s)
      [{:num (Integer/parseInt (apply str res))} []]
      (let [a (first s)]
        (if (or (= a \*)
                (= a \+))
          (if (empty? res)
            [{:oper (case a
                      \* (partial *)
                      \+ (partial +)
                      nil)} (rest s)]
            [{:num (Integer/parseInt (apply str res))} s])
          (recur (rest s) (conj res a)))))))

(defn next-token2 [s]
  (loop [s s res []]
    (if (empty? s)
      [(Integer/parseInt (apply str res))  []]
      (let [a (first s)]
        (if (or (= a \*)
                (= a \+))
          (if (empty? res)
            [a (rest s)]
            [(Integer/parseInt (apply str res))  s])
          (recur (rest s) (conj res a)))))))

(defn tokenize1 [s]
  (loop [s s res []]
    (if (empty? s)
      res
      (let [[token rem] (next-token s)]
        (recur rem (conj res token))))))

(defn tokenize2 [s]
  (loop [s s res []]
    (if (empty? s)
      res
      (let [[token rem] (next-token2 s)]
        (recur rem (conj res token))))))

(defn eval-expression1 [s]
  (loop [tokens (tokenize1 s) acc 0 oper nil]
    (if (empty? tokens)
      acc
      (let [token (first tokens)
            remaining-tokens (next tokens)]
        (if (:oper token)
          (recur remaining-tokens acc (:oper token))
          (if (and acc oper)
            (recur remaining-tokens (apply oper [acc (:num token)]) nil)
            (recur remaining-tokens (:num token) oper)))))))

(defn eval-expression2 [s]
  (loop [tokens (tokenize2 s)
         opers [\+ \*]]
    (if (empty? opers)
      (first tokens)
      (let [oper (first opers)
            i (.indexOf tokens oper)]
        (if (= -1 i)
          (recur tokens (rest opers))
          (let [arg1 (nth tokens (dec i))
                arg2 (nth tokens (inc i))
                val ((case oper
                       \* *
                       \+ +
                       nil) arg1 arg2)
                newtokens (concat (take (dec i) tokens)
                                  [val]
                                  (drop (+ i 2) tokens))]
            (recur newtokens opers)))))))

(defn eval-input [line eval-fn]
  (loop [line (str/replace line #"\s" "")]
    (let [n (str/replace line #"\([^\(\)]+\)"
                         #(str (eval-fn (butlast (rest %)))))]
      (if (= n line)
        (eval-fn line)
        (recur n)))))

(defn solve1 [filename eval-fn]
  (for [line (str/split (slurp filename) #"\n")]
    (eval-input line eval-fn)))

(defn solve []
  (let [result1 (solve1 "inputs/2020/day18.txt" eval-expression1)
        result2 (solve1 "inputs/2020/day18.txt" eval-expression2)]
    {:part1 (reduce + result1) :part2 (reduce + result2)}))


(defn status [] "*")
