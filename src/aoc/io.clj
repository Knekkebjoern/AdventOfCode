(ns aoc.io
  (:require [clojure.string :as str]))

(defn read-file [filename]
  (slurp filename))

(defn split-lines [s]
  (str/split-lines s))
