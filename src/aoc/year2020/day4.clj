(ns aoc.year2020.day4
  (:gen-class))

(require '[clojure.string :as str])

(defn get-input [filename]
  (for [entry (str/split (slurp filename) #"\r?\n\r?\n")]
    (into {} (map (fn [l]
                    (let [[k v]
                          (str/split l #":")]
                      [(keyword k) v]))
                  (str/split (str/replace entry #"\n" " ") #"\s+")))))

(defn is-valid [d]
  (or (= 8 (count (keys d)))
      (and (= 7 (count (keys d)))
           (not (contains? d :cid)))))

(defn is-valid2 [d]
  (and
   (is-valid d)
   (every? true?
           (for [[k v] d]
             (case k
               :byr (let [v (Integer/parseInt v)]
                      (and (>= v 1920)
                           (<= v 2002)))
               :iyr (let [v (Integer/parseInt v)]
                      (and (>= v 2010)
                           (<= v 2020)))
               :eyr (let [v (Integer/parseInt v)]
                      (and (>= v 2020)
                           (<= v 2030)))
               :hgt (let [[_ n t] (re-matches #"^(\d+)(cm|in)$" v)]
                      (if t
                        (let [n (Integer/parseInt n)]
                          (if (= "cm" t)
                            (and (>= n 150) (<= n 193))
                            (and (>= n 59) (<= n 76))))
                        nil))
               :hcl (some? (re-matches #"^\#[0-9a-f]{6}$" v))
               :pid (some? (re-matches #"^\d{9}$" v))
               :ecl (.contains [:amb :blu :brn :gry :grn :hzl :oth] (keyword v))
               :cid true
               false)))))

(defn solve []
  (let [data (get-input "inputs/2020/day4.txt")
        valid (filter is-valid data)
        valid2 (map is-valid2 data)]
    {:part1 (count valid)
     :part2 (count (filter true? valid2))}))

(defn status [] "*")
