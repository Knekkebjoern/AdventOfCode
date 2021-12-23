(ns year_2021.day18)
(require '[clojure.string :as str]
         '[clojure.zip :as z])

(def input (list
            [[[[2,5],4],[[1,0],[8,3]]],[[2,[2,4]],[1,[3,3]]]]
            [[[2,2],[[4,3],3]],[[[8,6],3],[3,7]]]
            [[[9,[4,1]],[9,0]],[6,[6,0]]]
            [[[3,9],[[4,4],[2,5]]],[[9,[8,4]],8]]
            [[[[0,0],9],[[9,3],[8,2]]],[2,[1,3]]]
            [[[8,4],6],[[5,1],[3,6]]]
            [[[6,[7,6]],[[2,6],5]],[[6,4],2]]
            [[1,[9,7]],[[[5,9],[9,5]],[[7,0],1]]]
            [[[[5,8],[9,4]],[[9,3],[7,8]]],8]
            [[[0,9],[[6,0],7]],[[[7,7],6],[[9,7],[0,4]]]]
            [[[[4,3],[9,5]],[7,[7,3]]],[[[2,8],9],4]]
            [[7,5],[8,1]]
            [[4,6],[[[0,6],6],[7,4]]]
            [[[1,8],[[1,4],[1,6]]],[3,4]]
            [[[6,5],[4,[7,3]]],[[[0,1],[8,4]],[4,8]]]
            [[5,1],[9,[9,[3,3]]]]
            [[[[7,0],[2,5]],1],[9,[[2,7],[4,4]]]]
            [[[[5,8],8],0],[8,[1,[2,5]]]]
            [8,[[5,4],7]]
            [[[9,8],[6,7]],[[2,[2,6]],[9,6]]]
            [[[[2,3],7],6],[[8,6],3]]
            [[[8,[7,2]],3],[[[3,9],4],[6,8]]]
            [9,[[[6,7],[6,0]],[[3,9],8]]]
            [[[7,7],[4,7]],[[[9,8],9],[9,[2,4]]]]
            [[[[5,0],1],[4,[4,8]]],[9,[6,7]]]
            [[[[9,2],5],[1,[5,8]]],[[9,[0,1]],[3,8]]]
            [[[5,[2,5]],8],[2,[0,[9,3]]]]
            [[7,[[8,4],[8,4]]],4]
            [[[[3,3],4],[[0,0],[5,5]]],[4,5]]
            [[[[9,3],[9,3]],2],[5,3]]
            [[[9,5],[1,4]],[[7,1],[3,[6,5]]]]
            [8,[[[1,1],[0,1]],[9,[3,6]]]]
            [[[[4,4],7],[0,3]],[1,5]]
            [[[3,[0,8]],8],[5,[7,5]]]
            [[[[9,6],2],7],[[5,[3,7]],0]]
            [4,9]
            [[[5,[1,3]],[[9,5],6]],[[[7,9],5],3]]
            [[[[3,9],[7,2]],[5,[8,8]]],[1,9]]
            [[[[7,8],8],[[9,0],[5,1]]],[6,[[1,0],[3,3]]]]
            [[[[5,8],1],[[8,6],[2,9]]],[[5,1],6]]
            [[1,7],[[5,[3,2]],4]]
            [[[[3,1],2],[0,8]],[3,[4,6]]]
            [[9,6],[0,[[5,2],[1,1]]]]
            [[[[1,8],8],[[9,0],3]],[[6,[2,8]],[[6,4],[6,0]]]]
            [[7,[[3,2],[9,0]]],[[[3,2],[2,8]],[[5,5],[9,2]]]]
            [[[[2,5],[3,1]],[7,[9,6]]],[[[7,0],7],[2,[9,1]]]]
            [[[[1,6],9],[1,[6,5]]],[[8,[4,1]],6]]
            [[[7,[4,6]],[[2,7],[6,6]]],[8,0]]
            [[9,7],[[[0,7],5],[[1,4],[1,3]]]]
            [[[1,[8,2]],[[0,6],[9,0]]],8]
            [[[4,0],[7,[3,3]]],[9,6]]
            [0,[[[6,9],7],[[0,6],1]]]
            [5,[[4,3],[[8,3],[5,7]]]]
            [[9,0],[0,[[7,8],[1,8]]]]
            [[[[4,3],[5,6]],2],[[2,3],1]]
            [4,[[9,9],[[1,8],[9,2]]]]
            [[[[6,9],5],1],[[[7,4],[8,1]],3]]
            [[8,[5,[2,6]]],[[[2,7],6],[6,0]]]
            [[[[6,8],8],6],[[[5,7],2],[[6,5],[3,0]]]]
            [[[1,[2,5]],3],[5,[4,[6,6]]]]
            [[[[4,9],8],1],[9,0]]
            [[1,[0,[5,7]]],[[1,[5,9]],[[3,2],[1,7]]]]
            [[[[2,9],[2,7]],[[4,2],5]],[[[9,1],[7,2]],[2,[7,5]]]]
            [[[[5,7],[8,9]],[5,[7,9]]],[[7,[6,6]],[7,[8,0]]]]
            [[[[6,6],[4,6]],[4,[7,8]]],[1,[[5,5],[1,9]]]]
            [[[[4,3],8],2],[[9,[4,0]],[8,[7,0]]]]
            [[2,[7,5]],[[[0,1],1],[8,[3,5]]]]
            [[[4,[4,2]],[[0,4],9]],[1,4]]
            [[[5,5],[5,6]],[[0,[4,2]],[[7,8],[5,6]]]]
            [2,[[0,[9,1]],[[1,7],[0,0]]]]
            [[[5,[4,8]],1],9]
            [8,[[2,1],[3,0]]]
            [[[[6,5],[1,1]],7],[[[7,5],3],[0,1]]]
            [[[[0,3],7],7],[[[4,8],[6,1]],[[6,1],9]]]
            [[[[4,8],9],[1,0]],[6,[4,[4,8]]]]
            [[[[8,0],[5,1]],6],1]
            [[[[6,6],[7,7]],[[4,3],[2,6]]],[[3,5],[[7,0],[7,3]]]]
            [[1,[5,8]],[[[3,7],[9,6]],[[4,8],[3,4]]]]
            [[[1,5],[8,2]],[[[3,1],5],[4,1]]]
            [[[[6,3],5],8],[[9,[3,6]],[[3,5],[6,9]]]]
            [[[7,[5,4]],[0,[6,0]]],[[[7,7],[1,1]],[[5,1],7]]]
            [[[1,5],[[8,6],0]],5]
            [[[[0,8],[6,0]],[[3,0],9]],[[[7,1],2],[4,2]]]
            [[[6,[8,7]],[2,[2,0]]],[9,[7,[6,6]]]]
            [3,[[7,[4,5]],[[8,5],4]]]
            [[[[8,0],[8,3]],[[5,4],[1,6]]],[[0,[8,5]],3]]
            [[[7,2],1],[9,[[3,8],4]]]
            [[4,[7,[9,9]]],[3,8]]
            [[[[7,1],9],[[6,9],[9,6]]],[2,0]]
            [[[[6,2],9],[3,[3,9]]],[[8,[3,4]],[3,7]]]
            [[4,9],[8,[5,[9,8]]]]
            [3,[[9,[9,7]],4]]
            [[[[5,9],6],[1,[3,1]]],[4,[1,[3,8]]]]
            [[[[7,6],2],3],[[0,[1,8]],[[4,9],[4,3]]]]
            [[3,[[8,1],[3,8]]],[[[2,0],[0,8]],[[7,0],9]]]
            [[[[9,7],[9,3]],[[5,8],6]],[[[6,2],0],[2,4]]]
            [[[8,[9,7]],[[5,1],[1,4]]],3]
            [[7,[[5,6],[2,7]]],[[[7,3],0],[1,[0,6]]]]
            [[2,[[5,5],2]],[[3,[7,2]],[[7,1],8]]]
            [[[[2,4],[6,8]],[0,[7,5]]],[[3,[2,5]],[7,7]]]
            ))

(def test-input (list
                 [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                 [[[5,[2,8]],4],[5,[[9,9],0]]]
                 [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                 [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                 [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                 [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                 [[[[5,4],[7,7]],8],[[8,3],8]]
                 [[9,3],[[9,9],[6,[4,9]]]]
                 [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                 [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]))

(def test-input2 (list [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                       [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                       [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                       [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                       [7,[5,[[3,8],[1,4]]]]
                       [[2,[2,2]],[8,[8,1]]]
                       [2,9]
                       [1,[[[9,3],9],[[9,0],[0,7]]]]
                       [[[5,[7,4]],7],1]
                       [[[[4,2],2],6],[8,7]]
                       ))

(def test-input3 (list [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                       [[[5,[2,8]],4],[5,[[9,9],0]]]
                       [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                       [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                       [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                       [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                       [[[[5,4],[7,7]],8],[[8,3],8]]
                       [[9,3],[[9,9],[6,[4,9]]]]
                       [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                       [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]))

(defn explode? [loc]
  (and
   (some? loc)
   (some? (z/path loc))
   (>= (count (z/path loc)) 4)
   (vector? (z/node loc))
   (every? number? (z/node loc))))

(defn get-simple [f loc]  
  (loop [loc loc]
    (cond (nil? loc) nil
          (z/end? loc) nil
          (nil? (z/node loc)) nil
          (number? (z/node loc)) loc
          :else (recur (f loc)))))

(defn get-explodable [loc]
  (if (or (z/end? loc)
          (explode? loc))
    loc
    (recur (z/next loc))))

(defn split [exp]
  (loop [loc (z/vector-zip exp)]
    (if (z/end? loc)
      (z/root loc)
      (if (and (number? (z/node loc))
               (>= (z/node loc) 10))
        (z/root (z/edit loc (fn [v & args] [(int (Math/floor (/ v 2)))
                                           (int (Math/ceil (/ v 2)))])))
        (recur (z/next loc))))))

(defn explode-exp [exp]
  (loop [loc (z/vector-zip exp)]
    (if (or (nil? loc)
            (z/end? loc))
      (if (z/end? loc)
        (z/root loc)
        loc)
      (do
        (if (explode? loc)
          (let [[l r] (z/node loc)
                p-loc (get-simple z/prev (z/prev loc))
                p-loc (if (some? p-loc)
                        (z/edit p-loc (fn [v & args] (+ l (z/node p-loc))))
                        p-loc)
                ;;
                m-loc (if (some? p-loc)
                        (get-explodable p-loc)
                        loc)
                m-loc (z/edit m-loc (fn [v & args] 0))
                ;;
                n-loc (get-simple z/next (z/next m-loc))
                n-loc (if (some? n-loc)
                        (z/edit n-loc (fn [v & args] (+ r (z/node n-loc))))
                        n-loc)]
            (if (some? n-loc)
              (z/root n-loc)
              (z/root m-loc)))            
          (recur (z/next loc)))))))

(defn reduce-exp [exp]
  (loop [exp exp]
    (let [tmp (explode-exp exp)
          tmp (if (= tmp exp)
                (split tmp)
                tmp)]
      (if (= tmp exp)
        exp
        (recur tmp)))))

(defn add-exp [a b]
  (reduce-exp [a b]))

(defn magnitude [exp]
  (let [loc (z/vector-zip exp)]
    (loop [loc loc]
      (if (z/end? loc)
        (z/root loc)
        (if (and (vector? (z/node loc))
                 (every? number? (z/node loc)))
          (recur  (z/vector-zip
                   (z/root (z/edit loc
                                   (fn [[l r] & args]
                                     (+ (* 3 l) (* 2 r)))))))
          (recur (z/next loc)))))))

(defn main []
  (let [res1 (reduce add-exp input)
        res2 (apply max (for [x input y input
                              :when (not= x y)]
                          (magnitude (add-exp x y))))]    
    (println "Part1" (magnitude res1))
    (println "Part2" res2)))

(defn tests []
  (assert (= (explode-exp [[[[[9,8],1],2],3],4])
             [[[[0,9],2],3],4]))
  (assert (= (explode-exp [7,[6,[5,[4,[3,2]]]]])
             [7,[6,[5,[7,0]]]]))
  (assert (= (explode-exp [[6,[5,[4,[3,2]]]],1])
             [[6,[5,[7,0]]],3]))
  (assert (= (explode-exp [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
             [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]))
  (assert (= (explode-exp [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
             [[3,[2,[8,0]]],[9,[5,[7,0]]]]))

  (assert (= (add-exp [[[[4,3],4],4],[7,[[8,4],9]]]
                      [1,1])
             [[[[0,7],4],[[7,8],[6,0]]],[8,1]]))
  (assert (= (reduce add-exp [[1,1] [2,2] [3,3] [4,4] [5,5]])
             [[[[3,0],[5,3]],[4,4]],[5,5]]
             ))
  (assert (= (reduce add-exp [[1,1] [2,2] [3,3] [4,4] [5,5] [6,6]])
             [[[[5,0],[7,4]],[5,5]],[6,6]])))
