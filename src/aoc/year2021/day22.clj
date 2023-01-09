(ns aoc.year2021.day22)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn normalize-inputs [inputs]
  (let [caplow-f (fn [i] (if (< i -50) -50 i))
        caphigh-f (fn [i] (if (> i 50) 50 i))]
    (loop [inputs inputs res []]
      (if (empty? inputs)
        res
        (let [[_ onoff xstart xend ystart yend zstart zend] (first inputs)
              [xstart xend ystart yend zstart zend] (map #(Integer/parseInt %) [xstart xend ystart yend zstart zend])]
          (if (or (some #(> % 50) [xstart ystart zstart])
                  (some #(> % 50) [xend yend zend]))
            ;; Outside range
            (recur (rest inputs) res)
            (recur (rest inputs) (conj res [onoff
                                            (caplow-f xstart) (caphigh-f xend)
                                            (caplow-f ystart) (caphigh-f yend)
                                            (caplow-f zstart) (caphigh-f zend)]))))))))


(defn get-input [filename]
  (let [lines (str/split (slurp filename) #"\n")]
    (for [line lines]
      (re-find #"^(on|off) x=(\-?\d+)\.\.(\-?\d+),y=(\-?\d+)\.\.(\-?\d+),z=(\-?\d+)\.\.(\-?\d+)" line))))

(defn solve1 []
  (count
   (let [inputs (normalize-inputs (get-input "inputs/2021/day22.txt"))]
     (loop [inputs inputs res (hash-set)]
       (if (empty? inputs)
         res
         (let [[onoff xstart xend ystart yend zstart zend] (first inputs)
               changes (apply hash-set (for [x (range xstart (inc xend))
                                             y (range ystart (inc yend))
                                             z (range zstart (inc zend))]
                                         [x y z]))
               new-res (if (= "on" onoff)
                         (clojure.set/union res changes)
                         (clojure.set/difference res changes))]
           (recur (rest inputs) new-res)))))))


;; Part 2

(defn normalize-inputs-2 [inputs]
  (loop [inputs inputs res []]
    (if (empty? inputs)
      res
      (let [[_ onoff xstart xend ystart yend zstart zend] (first inputs)
            [xstart xend ystart yend zstart zend] (map #(Integer/parseInt %) [xstart xend ystart yend zstart zend])]
        (recur (rest inputs) (conj res [onoff xstart xend ystart yend zstart zend]))))))

(defn overlap? [a b]
  (let [[_ axs axe ays aye azs aze] a
        [_ bxs bxe bys bye bzs bze] b]
    (and (or (< axs bxs axe)
             (< axs bxe axe))
         (or (< ays bys aye)
             (< ays bye aye))
         (or (< azs bzs aze)
             (< azs bze aze)))))

(defn intersect [a b]
  (if (not (overlap? a b))
    [a]
    (let [[aonoff axs axe ays aye azs aze] a
          [bonoff bxs bxe bys bye bzs bze] b
                                        ; Find overlap
          [nxs nxe] (cond
                      (or (<= axs axe bxs bxe)
                          (<= bxs bxe axs axe)) [axs axe] ; no overlap
                      (<= axs bxs bxe axe) [bxs bxe]
                      (<= axs bxs axe bxe) [bxs axe]
                      (<= bxs axs bxe axe) [axs bxe]
                      :default [axs axe])
          [nys nye] (cond
                      (or (<= ays aye bys bye)
                          (<= bys bye ays aye)) [ays aye] ; no overlap
                      (<= ays bys bye aye) [bys bye]
                      (<= ays bys aye bye) [bys aye]
                      (<= bys ays bye aye) [ays bye]
                      :default [ays aye])
          [nzs nze] (cond
                      (or (<= azs aze bzs bze)
                          (<= bzs bze azs aze)) [azs aze] ; no overlap
                      (<= azs bzs bze aze) [bzs bze]
                      (<= azs bzs aze bze) [bzs aze]
                      (<= bzs azs bze aze) [azs bze]
                      :default [azs aze])
          new-cuboids [[aonoff axs axe nye aye azs aze :top] ; top slice
                       [aonoff axs axe nys nye azs nzs :front] ; front face

                       [aonoff nxe axe nys nye nzs nze :right] ; right face

                       [aonoff axs nxs nys nye azs aze :left] ; left face
                       [aonoff axs axe nys nye nze aze :back] ; back face
                       [aonoff axs axe ays nys azs aze :bottom] ; bottom slice
                       [bonoff nxs nxe nys nye nzs nze]]
          valid-f (fn [x]
                    (every? true?
                            (map #(not= (first %) (second %))
                                 (partition 2 (drop 1 x)))))
          new-cuboids (filter valid-f new-cuboids)]
      new-cuboids)))

(defn process-step [data cuboid]
  (loop [data data res []]
    (if (empty? data)
      (concat res [cuboid])
      (let [new-cuboids (intersect (first data) cuboid)]
        (recur (rest data) (concat res new-cuboids))))))

(defn volume [[_ axs axe ays aye azs aze _]]
  (* (- axe axs) (- aye ays) (- aze azs)))

(defn solve2 []
  (let [inputs (normalize-inputs-2 (get-input "inputs/day22-test2.txt"))
        data (loop [inputs (rest inputs) res [(first inputs)]]
               (if (empty? inputs)
                 res
                 (let [new-res (process-step res (first inputs))
                       ;; _ (prn "input" (first inputs))
                       ;; _ (prn  "res" new-res)
                       ;; _ (println)
                       ]
                   (recur (rest inputs) new-res))))
        on (filter #(= (first %) "on") data)
        ;_ (prn (for [x on] (prn x (volume x))))
        totalon (reduce + (map volume on))]
    (println totalon)))

(def test-data [["on" 0 10 0 10 0 10]])

(defn tests []
  (assert (= (process-step test-data ["on" 0 5 0 5 0 5])
             '(["on" 0 10 6 10 0 10 :top] ["on" 6 10 0 6 0 6 :right] ["on" 0 10 0 5 5 10 :back] ["on" 0 5 0 5 0 5] ["on" 0 5 0 5 0 5]))))

(defn solve []
  (let [part1 (solve1)]
    {:part1 part1 :part2 nil}))

(defn status [] "!")
