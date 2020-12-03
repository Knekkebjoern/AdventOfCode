(ns day2.core
  (:gen-class))

(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data (map
              #(let [[line minval maxval letter password] (re-matches #"^(\d+)-(\d+)\s+([a-z]{1}):\s+([a-z]+)$" %)]
                 {:min (Integer/parseInt minval)
                  :max (Integer/parseInt maxval)
                  :letter (char (first (.getBytes letter)))
                  :password password
                  :line line
                  :frequencies (frequencies password)})
              lines)]
    data))

(defn valid-password [data]
  (let [cnt (get (:frequencies data) (:letter data))]
    (and (not (nil? cnt))
         (<= cnt (:max data))
         (>= cnt (:min data)))))

(defn valid-password2 [data]
  (apply distinct? (map #(= (:letter data)
                            (nth (:password data) (dec (get data %))))
                        [:min :max])))

(defn -main
  [& args]
  (let [input (get-input "input.txt")]
    (println "Part 1 Correct passwords:" (count (filter valid-password input)))
    (println "Part 2 Correct passwords:" (count (filter valid-password2 input)))))
