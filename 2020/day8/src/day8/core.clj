(ns day8.core
  (:gen-class))


(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn get-input [filename]
  (apply merge
   (for [line (str/split (slurp filename) #"\n")])))

(defn -main
  [& args]
  (println "Hello, World!"))
