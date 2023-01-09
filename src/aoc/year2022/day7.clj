(ns aoc.year2022.day7)
(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

(defn transpose [m]
  (apply mapv vector m))

(defn get-input [filename]
  (loop [lines (str/split (slurp filename) #"\n") dirs [] data {}]
    (if (empty? lines)
      data
      (let [line (first lines)
            newdirs (if-let [[_ dir] (re-find #"^\$ cd (.*)$" line)]
                      (case dir
                        ".." (vec (butlast dirs))
                        "/" ["/"]
                        (conj dirs dir))
                      dirs)
            newdata (if (not= dirs newdirs)
                      (update data newdirs #(assoc % "mytmp.txt" 0))
                      data)
            newdata (if-let [[_ size filename] (re-find #"^(\d+) (.*?)$" line)]
                      (update newdata newdirs
                              #(assoc % filename (Integer/parseInt size)))
                      newdata)]
        (recur (rest lines) newdirs newdata)))))

(defn dirsize [data dir]
  (let [allpaths (keys data)
        subdirs (filter #(and (= dir (take (count dir) %))
                              (= (count %) (inc (count dir)))) allpaths)
        subtotal (reduce + (for [d subdirs]
                             (dirsize data d)))
        total (if-let [files (get data dir {})]
                (reduce + (vals files)))]
    (+ total subtotal)))

(defn solve1 [data]
  (reduce + (for [d (keys data)
                  :let [size (dirsize data d)]
                  :when (<= size 100000)]
              size)))

(defn solve2 [data]
  (let [have 70000000
        need 30000000
        used (dirsize data ["/"])
        want (- need (- have used))
        dirs (for [d (keys data)
                   :let [size (dirsize data d)]
                   :when (>= size want)]
               size)]
    (apply min dirs)))

(defn solve []
  (let [input (get-input "inputs/2022/day7.txt")
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
