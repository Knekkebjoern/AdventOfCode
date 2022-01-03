(ns year_2021.day20)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        algo (first lines)
        lines (drop 2 lines)
        max-y (dec (count lines))
        max-x (dec (count (first lines)))
        image-data (apply merge
                          (for [y (range (count lines))
                                :let [line (nth lines y)]]
                            (apply merge
                                   (for [x (range (count line))]
                                     {[x y] (nth line x)}))))]
    [algo {:data image-data
           :default \.
           :min-x 0 :max-x max-x
           :min-y 0 :max-y max-y}]))

(defn get-surrounding [[x y] image]
  (let [{data :data} image]
    (flatten
     (for [yy [(dec y) y (inc y)]]
       (for [xx [(dec x) x (inc x)]]
         (get data [xx yy] (:default image)))))))

(defn to-decimal [m]
  (Integer/parseInt (apply str
                           (map #(case %
                                   \. 0
                                   \# 1) m))
                    2))

(defn print-image [image]
  (let [{data :data
         min-x :min-x max-x :max-x
         min-y :min-y max-y :max-y} image]
    (do
      (println)
      (doall (for [y (range min-y (inc max-y))]
               (println (apply str
                               (for [x (range min-x (inc max-x))]
                                 (get data [x y] (:default image)))))))))
  image)

(defn process-image [image algo]
  (let [{data :data
         default :default
         min-x :min-x max-x :max-x
         min-y :min-y max-y :max-y} image
        points (for [x (range (- min-x 2) (+ 3 max-x))
                     y (range (- min-y 2) (+ 3 max-y))]
                 [x y])
        new-data (apply merge (for [point points
                                    :let [surrounding (get-surrounding point image)
                                          ind (to-decimal surrounding)
                                          new-val (nth algo ind)]]
                                {point new-val}))
        ]
    {:data new-data
     :default (nth algo (to-decimal (take 9 (repeat default))))
     :min-x (- min-x 2) :max-x (+ max-x 2)
     :min-y (- min-y 2) :max-y (+ max-y 2)}))

(defn count-lit [{data :data}]
  (count (filter #(= % \#) (vals data))))

(defn solve1 []
  (let [[algo image] (get-input "inputs/day20.txt")]
    (-> image
        (process-image algo)
        (process-image algo)        
        (count-lit))))

(defn solve2 []
  (let [[algo image] (get-input "inputs/day20.txt")]
    (loop [image image i 50]
      (if (zero? i)
        (prn (count-lit image))
        (recur (process-image image algo) (dec i))))))
