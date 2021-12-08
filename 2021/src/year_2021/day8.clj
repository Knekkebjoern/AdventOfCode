(ns year_2021.day8)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data  (for [line lines
                    :let [[signal output] (str/split line #"\s+\|\s+")
                          patterns (str/split signal #"\s+")
                          values (str/split output #"\s+")]]
                {:patterns (map set patterns) :values (map set values)})]
    data))

(defn filter-count [n coll]
  (filter #(= n (count %)) coll))

(defn decode [patterns]
  (let [one (first (filter-count 2 patterns))
        four (first (filter-count 4 patterns))
        seven (first (filter-count 3 patterns))
        eight (first (filter-count 7 patterns))
        aa (clojure.set/difference seven one)
        bb_dd (clojure.set/difference four one)
        bb_dd_aa (set (concat bb_dd aa))
        five (first (filter #(clojure.set/subset? bb_dd_aa %)
                           (filter-count 5 patterns)))
        six_zero_or_nine (filter-count 6 patterns)
        zero_or_nine (filter #(clojure.set/subset? one %) six_zero_or_nine)
        nine (first (filter #(clojure.set/subset? bb_dd %) zero_or_nine))
        zero (first (filter #(not (= nine %)) zero_or_nine))
        six (first (filter #(and (not (= nine %))
                                 (not (= zero %))) six_zero_or_nine))

        two_or_three (filter #(not (= five %)) (filter-count 5 patterns))
        three (first (filter #(clojure.set/subset? one %) two_or_three))
        two (first (filter #(not (= three %)) two_or_three))]
    {zero "0" one "1" two "2" three "3" four "4"
     five "5" six "6" seven "7" eight "8" nine "9"}))

(defn solve1 [data res]
  (if (empty? data)
    (count (filter #(not (nil? %)) (flatten res)))
    (let [entry (first data)
          mappings (decode (:patterns entry))
          code (for [d (:values entry)]
                 (get mappings d nil))]
      (recur (rest data) (conj res code)))))

(defn solve2 [data res]
  (if (empty? data)
    (reduce + (for [r res] (Integer/parseInt (apply str r))))
    (let [entry (first data)
          mappings (decode (:patterns entry))
          code (for [d (:values entry)]
                 (get mappings d nil))]
      (recur (rest data) (conj res code)))))

(let [input1 (get-input"inputs/day8_input1.txt")
      input2 (get-input"inputs/day8_input2.txt")
      part1 (solve1 input1 [])
      part2 (solve2 input1 [])]
  (println "Part1: " part1)
  (println "Part2: " part2))

