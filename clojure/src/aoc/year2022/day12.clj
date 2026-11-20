(ns aoc.year2022.day12)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[clojure.data.priority-map :refer [priority-map]])
(require '[quil.core :as q])
(require '[quil.middleware :as m])
(import (java.util PriorityQueue))

(def ^:private inf (Long/MAX_VALUE))

(defn index-of [e coll] (first (for [[k v] coll :when (= v e)] k)))

(defn valid-neighbor-one-up? [g a b]
  (let [va (get g a)
        vb (get g b)]
    (<= (- vb va) 1) ; one higher, equal or lower elevation
    ))

(defn valid-neighbor-one-down? [g a b]
  (let [va (get g a)
        vb (get g b)]
    (<= (- va vb) 1) ; one lower, equal or higher elevation
    ))

(defn get-neighbors [g valid-neighbor-f [x y]]
  (for [neighbor [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
        :when (and (contains? g neighbor)
                   (valid-neighbor-f g [x y] neighbor))]
    neighbor))

(defn init-state [state]
  (let [{:keys [g source target]} state]
    (assoc state
           :dist (-> (zipmap (keys g) (repeat inf))
                     (assoc source 0))
           :prev (-> (zipmap (keys g) (repeat nil))
                     (dissoc target))
           :Q (priority-map source 0))))

(defn get-input [filename]
  (loop [lines (str/split (slurp filename) #"\n") row 0 cols 0 data {}]
    (if (empty? lines)
      (let [source (index-of 0 data)
            target (index-of 27 data)]
        (init-state  {:g data :rows row :cols cols
                      :source source
                      :target target
                      :valid-neighbor-f valid-neighbor-one-up?}))
      (let [rowdata (apply merge
                           (for [col (range (count (first lines)))
                                 :let [line (first lines)
                                       v (nth line col)
                                       v (case v
                                           \S 0
                                           \E 27
                                           (- (int v) 96))]]
                             {[col row] v}))
            newdata (apply merge data rowdata)]
        (recur (rest lines) (inc row) (count (first lines)) newdata)))))

(defn dijkstra-step [state]
  (let [{:keys [g source target dist prev Q valid-neighbor-f]} state]
    (if (empty? Q)
      state
      (let [[u dist_u] (peek Q)
            Q' (pop Q)
            neighbors (get-neighbors g valid-neighbor-f u)
            alt (inc dist_u)
            candidates (for [v neighbors
                             :when (< alt (get dist v))] v)
            dist' (apply merge dist (for [v candidates] {v alt}))
            prev' (apply merge prev (for [v candidates] {v u}))
            Q' (into Q' (for [v candidates] [v alt]))]
        (assoc state :dist dist' :prev prev' :Q Q')))))

(defn steps-from [prev source target]
  (loop [curr target res #{}]
    (if (= source curr)
      (- (count res) 2)
      (recur (get prev curr) (conj res curr)))))

(defn solve1 [state]
  (loop [state state]
    (let [state' (dijkstra-step state)]
      (if (empty? (:Q state'))
        state'
        (recur state')))))

(defn solve2 [state]
  (let [state' (solve1 (init-state (assoc state
                                         :source (:target state)
                                         :target (:source state)
                                         :valid-neighbor-f valid-neighbor-one-down?)))
        sources (set (for [[pos v] (:g state) :when (= 1 v)] pos))
        dists (:dist state')]
    (- (first (sort (for [pos sources :when (contains? sources pos)] (get dists pos))))
       2)))

;; (defn setup []
;;   (q/frame-rate 30)
;;   (q/background 200)
;;   (get-input "inputs/day12.txt"))


;; (defn update-state [state]
;;   (loop [state state i 100]
;;     (if (empty? (:Q state))
;;       (do
;;         (print-result state)
;;         (q/no-loop)
;;         state)
;;       (if (zero? i)
;;         state
;;         (recur (dijkstra-step state) (dec i))))))

;; (defn draw-path [state]
;;   (q/fill 255 0 0)
;;   (let [{:keys [g Q prev target source cols rows]} state
;;         [curr _] (peek Q)
;;         curr (or curr target)
;;         grid-width (/ (q/width) cols)
;;         grid-height (/ (q/height) rows)
;;         grid-spacing 2]
;;     (loop [curr curr]
;;       (if (or (nil? curr)
;;               (= curr source)
;;               (nil? (get prev curr)))
;;         nil
;;         (let [[x y] (get prev curr)]
;;           (q/rect (+ (* x grid-width) grid-spacing)
;;                   (- (* y grid-height) grid-spacing)
;;                   (- grid-width grid-spacing)
;;                   (- grid-height grid-spacing))
;;           (recur [x y]))))))

;; (defn draw [state]
;;   (q/stroke 0)
;;   (q/stroke-weight 2)
;;   (q/background 0 0 0)

;;   (let [{:keys [rows cols g source target prev dist Q step]} state
;;         grid-width (/ (q/width) cols)
;;         grid-height (/ (q/height) rows)
;;         grid-spacing 2]
;;     (dorun (for [x (range cols) y (range rows)]
;;              (do
;;                (cond
;;                  (= [x y] source) (q/fill 0 0 0)
;;                  (= [x y] target) (q/fill 255 255 255)
;;                  (contains? Q [x y]) (q/fill 0 255 255)
;;                  (and (contains? dist [x y])
;;                       (< (get dist [x y]) inf)) (q/fill 32)
;;                  :default (q/fill 128))
;;                (q/rect (+ (* x grid-width) grid-spacing)
;;                        (- (* y grid-height) grid-spacing)
;;                        (- grid-width grid-spacing)
;;                        (- grid-height grid-spacing)))))
;;     (draw-path state)))

;; (q/defsketch landscape
;;   :title "Landscape"
;;   :settings #(q/smooth 2)
;;   :setup setup
;;   :update update-state
;;   :draw draw
;;   :size [1200 600]
;;   :middleware [m/fun-mode])


(defn solve []
  (let [input (get-input "inputs/2022/day12.txt")
        state (solve1 input)
        part1 (steps-from (:prev state) (:source state) (:target state))
        part2 (solve2 input)]
    {:part1 part1 :part2 part2}))

(defn status [] "*")
