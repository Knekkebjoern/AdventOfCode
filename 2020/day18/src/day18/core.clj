(ns day18.core
  (:require [clojure.string :as str]))


;; (defn tokenize [s]
;;   (let [tokens (str/split (str/replace s #"[\(\)]" "") #"[\+\*]")
;;         a (prn "TOKENS" tokens)])
;;   (for [token tokens]
;;     (case token
;;       "*" {:tmp})))

(defn next-token [s]
  (loop [s s res []]
    (if (empty? s)
      [{ :num (Integer/parseInt (apply str res)) } []]
      (let [a (first s)]
        (if (or (= a \*)
                (= a \+))
          (if (empty? res)            
            [ {:oper (case a
                       \* (partial *)
                       \+ (partial +)
                       nil)} (rest s)]
            [{ :num (Integer/parseInt (apply str res)) } s])
          (recur (rest s) (conj res a)))))))

(defn tokenize [s]
  (loop [s s res []]
    (if (empty? s)
      res
      (let [[token rem] (next-token s)]
        (recur rem (conj res token))))))

(defn eval-expression [s]
  (loop [tokens (tokenize s) acc 0 oper nil]
    (if (empty? tokens)
      acc
      (let [token (first tokens)
            remaining-tokens (next tokens)]
        (if (:oper token)
          (recur remaining-tokens acc (:oper token))
          (if (and acc oper)
            (recur remaining-tokens (apply oper [acc (:num token)]) nil)
            (recur remaining-tokens (:num token) oper)))))))


(defn eval-input [line]
  (loop [line (str/replace line #"\s" "")]
    (let [n (str/replace line #"\([^\(\)]+\)"
                         #(str (eval-expression (butlast (rest %)))))]
      (if (= n line)
        (eval-expression line)
        (recur n)))))


(defn get-input [filename]
  (for [line (str/split (slurp filename) #"\n")]
    (eval-input line)))

(defn -main
  [& args]
  (let [result1 (get-input "input.txt")]
    (println "Result1:" (reduce + result1))))
