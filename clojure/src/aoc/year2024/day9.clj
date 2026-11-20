(ns aoc.year2024.day9
  (:require
   [aoc.io :as io]))

(comment
  (aoc.core/fetch-input! 2024 9))

(def input-filename "inputs/2024/day9.txt")
;;(def input-filename "inputs/2024/day9-test.txt")

(defn get-input []
  (doall (vec (filter number? (for [c (io/read-file input-filename)]
                                (parse-long (str c)))))))

(defn expand [data]
  (flatten (map-indexed (fn [idx v]
                          (if (even? idx)
                            (repeat v (/ idx 2))
                            (repeat v \.))) data)))

(defn reorg [data]
  (let [total-spaces (count (filter #(= %1 \.) data))]
    (loop [vs data
           rev (filter #(not (= %1 \.)) (reverse vs))
           spaces total-spaces
           res []]
      (if (zero? spaces)
        (take (count data)
              (concat (take (- (count data) total-spaces) res)
                      (repeat (count data)  \.)))
        (if (= (first vs) \.)
          (recur  (rest vs)  (doall (drop-last (rest rev))) (dec spaces) (conj res (first rev)))
          (recur (rest vs) rev spaces (conj res (first vs))))))))

(defn checksum [data]
  (reduce + (map-indexed (fn [idx v]
                           (if (or (= \. v) (= "." v))
                             0
                             (* idx (parse-long (str v))))) data)))

(defn solve1 [input]
  (checksum  (reorg (expand input))))

(defn is-space? [item]
  (= :space (:t item)))

(defn consolidate-spaces [disk]
  (loop [disk disk
         space-id 0
         idx 0
         res []]
    (if (empty? disk)
      res
      (let [item (first disk)]
        (if-not (is-space? item)
          (recur (rest disk) space-id (inc idx) (conj res (assoc item :index idx)))
          (let [blocks (reduce + (map :blocks (take-while is-space? disk)))]
            (recur (drop-while is-space? disk)
                   (inc space-id)
                   (inc idx)
                   (conj res (assoc item :blocks blocks :space-id space-id :index idx)))))))))

(defn get-disk [disk]
  (flatten (doall (for [x disk]
                    (if (= :file (:t x))
                      (repeat (:blocks x) (str (:file-id x)))
                      (repeat (:blocks x) (str \.)))))))

(defn move-file [disk file-id]
  ;;(print-disk disk)
  (let [file (first (filter #(= file-id (:file-id %1)) disk))
        space (first (filter #(and (map? %1)
                                   (= :space (:t %1))
                                   (>= (:blocks %1) (:blocks file))) disk))
        before? (if space (< (:index space) (:index file)) nil)]
    (if-not (and space before?)
      disk
      (loop [disk disk
             res []]
        (if (empty? disk)
          (consolidate-spaces res)
          (let [item (first disk)
                leftover (- (:blocks space) (:blocks file))
                res (cond
                      ;; Move item
                      (= (:space-id item) (:space-id space)) (if (pos? leftover)
                                                               (conj res file (assoc space
                                                                                     :blocks leftover))
                                                               (conj res file))
                      ;; Replace with space
                      (= (:file-id item) (:file-id file)) (conj res {:blocks (:blocks item) :t :space})
                      :else (conj res item))]
            (recur (rest disk) res)))))))

(defn checksum-2 [disk]
  (checksum (get-disk disk)))

(defn solve2 [input]
  (loop [disk (vec (map-indexed (fn [idx v] (if (even? idx)
                                              {:t :file :blocks v :file-id (/ idx 2) :index idx}
                                              {:t :space :blocks v :space-id (int (/ idx 2)) :index idx})) input))
         file-ids (reverse (range (/ (count input) 2)))]
    (if (empty? file-ids)
      (checksum-2 disk)
      (recur (move-file disk (first file-ids)) (rest file-ids)))))

(defn solve []
  (let [[part1 part2] [nil nil]
        input (get-input)
        part1 (solve1 input)
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(println (solve))

(defn status [] "*")
