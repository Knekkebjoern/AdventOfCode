(ns year_2021.day15)
(require '[clojure.string :as str])
(import (java.util PriorityQueue))

(defn get-input [filename]
  (let [all-lines (str/split (slurp filename) #"\n")
        data  (loop [lines all-lines y 0 res {}]
                (if (empty? lines)
                  {:data res :end [(dec (count (first all-lines)))
                                   (dec (count all-lines))]}  
                  (let [line (first lines)
                        d (apply merge
                                 (for [x (range (count line))
                                       :let [v (Integer/parseInt (str (nth line x)))]]
                                   {[x y] v}))]
                    (recur (rest lines) (inc y) (merge res d)))))
        end [(dec (count (first data))) (dec (count data))]]
    data))


(defn expand-point [[[x y] risk] width height]
  (apply merge
         (for [yy (range 0 5)
               xx (range 0 5)
               :let [nx (+ x (* width xx))
                     ny (+ y (* height yy))
                     new-risk (first
                               (take 1 (drop (+ xx yy (dec risk))
                                             (cycle (range 1 10)))))]
               :when (not= [nx ny] [x y])
               ]
           {[nx ny] new-risk}))
  )

(defn expand-map [{data :data [endx endy] :end }]
  {:data (apply merge data (for [point data]
                             (expand-point point (inc endx) (inc endy))))
   :end [(dec (* (inc endx) 5))
         (dec (* (inc endy) 5))]})


(def ^:private inf (Long/MAX_VALUE))

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g [x y]] (apply merge
                    (for [neighbor [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
                          :when (contains? g neighbor)]
                      {neighbor (get g neighbor)})))
  ([g n uv] (reduce-kv (fn [m k v] (if (.contains uv k)
                                    (assoc m k v)
                                    m))
                       {} (neighbors g n))))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs curr unvisited]
  (let [curr-cost (get costs curr)
        neighbors (neighbors g curr unvisited)
        res (reduce
             (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
             costs
             neighbors)]
    res))

;; Borrowed from https://gist.github.com/loganlinn/5437067
(defn dijkstra
  "Returns a mapping of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
  [g src target]
  (let [unvisited (PriorityQueue. )]
    (do
      (dorun (map #(.add unvisited %) (keys g)))
      (.remove unvisited src)
      (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
             curr src
             unvisited unvisited]
        (if (or (zero? (.size unvisited)) (= inf (costs curr)))
          costs
          (let [costs' (update-costs g costs curr unvisited)
                curr' (.peek unvisited)
                s (.size unvisited)]
            (if (zero? (mod s 100))
                    (prn "unvisited" (.size unvisited))
                    nil)
            (if (= target curr)
              (get costs' target)
              (do
                (.remove unvisited curr')
                (recur costs'
                       curr'
                       unvisited)))))))))


(defn main []
  (let [;; part1  (solve1 [0 0] (:end input1) (:data input1))
        ;; part1 (reduce + (for [p part1] (:risk p)))
        ;; part1 (- part1 (get (:data input1) [0 0]))

        ;;part2  (solve1 [0 0] (:end new-map) (:data new-map) false)
        ;;a (prn "DONE 1")
        expanded-map (expand-map (get-input "inputs/day15_input1.txt"))
        part2 (dijkstra (:data expanded-map) [0 0] (:end expanded-map))
        part2 (get part2 (:end expanded-map))
        
        ]
    ;;(println "Part1: " part1)
    (println "Part2: " part2)
    ))
