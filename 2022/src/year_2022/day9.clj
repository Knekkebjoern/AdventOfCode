(ns year_2022.day9)
(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])

; (:R :R :R :R :U :U :U :U :L :L :L :D :R :R :R :R :D :L :L :L :L :L :R :R)
(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]    
    (flatten (map #(if-let [[_ d c] (re-matches #"^(.*?) (\d*)$" %)]
                     (repeat (Integer/parseInt c) (keyword d))
                     []) lines))))


(defn move-head [[x y] step]
  (case step
    :U [x (inc y)]
    :D [x (dec y)]
    :L [(dec x) y]
    :R [(inc x) y]))

(defn adjacent? [[hx hy] [tx ty]]
  (and (<= (Math/abs (- hx tx)) 1)
       (<= (Math/abs (- hy ty)) 1)))

(defn move-tail [[hx hy] [tx ty]]
  (if (adjacent? [hx hy] [tx ty])
    [tx ty]
    (let [dx (Math/abs (- hx tx))
          dy (Math/abs (- hy ty))]
      (cond
        (= [hx hy] [tx ty]) [tx ty] ; same location
        (= hy ty) (if (< hx tx) ; same row
                    [(inc hx) ty]
                    [(dec hx) ty])
        (= hx tx) (if (< hy ty) ; same column
                    [tx (inc hy)]
                    [tx (dec hy)])
        (= dx dy) [(if (< hx tx)
                     (inc hx)
                     (dec hx))
                   (if (< hy ty)
                     (inc hy)
                     (dec hy))]
        (< 1 dx) (if (< hx tx) ; diagonal row
                   [(inc hx) hy] 
                   [(dec hx) hy])
        (< 1 dy) (if (< hy ty) ; diagonal column
                                     [hx (inc hy)]
                                     [hx (dec hy)])
        ))))

(defn solve1 [data]
  (loop [steps data head [0 0] tail [0 0] visited #{[0 0]}]
    (if (empty? steps)
      (count visited)
      (let [newhead (move-head head (first steps))
            newtail (move-tail newhead tail)]
        (recur (rest steps) newhead newtail (conj visited newtail))))))

(defn show-knots [knots]
  (let [maxx (+ 2 (apply max (map first knots)))
        maxy (+ 2 (apply max (map second knots)))
        minx (- (apply min (map first knots)) 2)
        miny (- (apply min (map second knots)) 2)
        tmp (for [y (reverse (range miny maxy))
                  x (range minx maxx)]
              (if (contains? knots [x y])
                \#
                \.))]
    (do
      (dorun
       (for [a (partition (- maxx minx) tmp)]
         (println (apply str a)))))
    nil))

(defn solve2 [data]
  (loop [steps data
         knots (repeat 10 [0 0])
         visited #{[0 0]}]    
    (if (empty? steps)
      (do
        (count visited))
      (let [newknots (loop [elems (rest knots)
                            res [(move-head (first knots) (first steps))]]
                       (if (empty? elems)
                         res
                         (let [head (last res)
                               tail (move-tail head (first elems))]
                           (recur (rest elems) (conj res tail)))))
            newtail (last newknots)]
        (recur (rest steps) newknots (conj visited newtail))))))



(let [input (get-input "inputs/day9.txt")
      part1 (solve1 input)
      part2 (solve2 input)]
  (println "Part1: " part1)
  (println "Part2: " part2))

