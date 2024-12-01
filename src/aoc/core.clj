(ns aoc.core
  (:require [clj-http.client :as http]
            [clojure.java.io :as io])
  (:gen-class))

(def YEARS (range 2015 2024))
(def DAYS (range 1 26))

(defn get-input-filename [year day]
  (str "inputs/" year "/day" day ".txt"))

(defn fetch-input! [year day]
  ;; slurp will throw exception if file doesn't exist
  ;; .aoc_session_id should be at the repository root and contain only the cookie session value
  (let [session-id (slurp ".aoc_session_id")
        _ (println "using session id: " session-id)
        res (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                      {:headers {"cookie" (str "session=" session-id)}})]
    (if (= (:status res) 200)
      (let [content (:body res)
            filename (str "inputs/" year "/day" day ".txt")]
        (spit filename content)
        (println "Wrote" (count content) "bytes to" filename)))))

(defn update-inputs! [force]
  (for [year YEARS
        day DAYS
        :let [filename (get-input-filename year day)]
        :when (or force (not (.exists (io/file filename))))]
    (fetch-input! year day)))

(defn get-problem-fns []
  (apply merge (for [year YEARS
                     day DAYS
                     :let [ns-name (str "year" year "/" "day" day)
                           ns-sym (symbol (str "aoc.year" year ".day" day))]]
                 (try
                   (do
                     (load ns-name)
                     (let [interns (ns-interns ns-sym)]
                       {[year day] (into {} (for [x ['solve 'status]]
                                              {(keyword x) (get interns x)}))}))
                   (catch Exception _ {[year day] nil})))))

(defn print-status []
  (let [problem-fns (get-problem-fns)]
    (loop [years YEARS res []]
      (if (empty? years)
        (dorun (map println res))
        (let [year (first years)
              status (str year " "
                          (apply str
                                 (for [day DAYS
                                       :let [day-fns (get problem-fns [year day])]]
                                   (cond
                                     (nil? day-fns) "."
                                     (nil? (:status day-fns)) "?"
                                     :else ((:status day-fns))))))]
          (recur (rest years) (conj res status)))))))

(defn solve-problem [year day]
  (let [problem-fns (get-problem-fns)
        fns (get problem-fns [year day])]
    (println "Solving" year "/" day)
    (if fns
      (let [f (:solve fns)
            res (if f (f) {:part1 "NOT IMPLEMENTED"
                           :part2 "NOT IMPLEMENTED"})]
        (println "    Part1: " (:part1 res))
        (println "    Part2: " (:part2 res)))
      (println "    ### No solutions"))))

(defn solve-all []
  (loop [problems (for [year YEARS day DAYS] [year day])]
    (if (empty? problems)
      nil
      (let [[year day] (first problems)]
        (solve-problem year day)
        (recur (rest problems))))))

(defn -main [& args]
  (cond
    (contains? (set args) "status") (print-status)))
