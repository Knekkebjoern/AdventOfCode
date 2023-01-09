(ns aoc.year2021.day12)
(require '[clojure.string :as str])

(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")
        data   (apply concat
                      (for [line lines
                                  :let [[a b] (str/split line #"-")]]
                        (cond
                          (or (= b "start")
                              (= a "end")) [[b a]]
                          (or (= a "start")
                              (= b "end")) [[a b]]
                          :else [[a b] [b a]])))]
    data))

(defn get-exits-from [data room]
  (for [[a b] data
        :when (= a room)] b))

(defn is-small-room? [room]
  (= (str/lower-case room) room))

(defn can-visit-1? [room path]
  (if (is-small-room? room)
    (let [small-rooms (filter is-small-room? path)]
      (not (contains? (frequencies small-rooms) room)))
    true))

(defn can-visit-2? [room path]
  (if (is-small-room? room)
    (let [small-rooms (filter is-small-room? path)
          visited-twice? (not= 0 (count (filter #(>= % 2) (vals (frequencies small-rooms)))))]
      (if visited-twice?
        ;; this room is not already in path
        (not (contains? (frequencies small-rooms) room))
        true))
    true))

(defn get-paths [can-visit-fn visited room data]
  (let [next-rooms (get-exits-from data room)]
    (if (or (empty? next-rooms)
            (not (can-visit-fn room visited)))
      (list (list room))
      (let [new-visited (conj visited room)
            paths (apply concat (for [next-room next-rooms]
                                  (get-paths can-visit-fn new-visited next-room data)))
            paths (for [path paths]
                    (conj path room))]
        paths))))

(defn solve1 [filter-fn data]
  (let [paths (get-paths filter-fn (list) "start" data)
        res (filter #(= "end" (last %)) paths)]
    (count res)))

(defn solve []
  (let [input (get-input"inputs/2021/day12.txt")
        part1 (solve1 can-visit-1? input)
        part2 (solve1 can-visit-2? input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
