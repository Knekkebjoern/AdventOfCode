(ns aoc.year2021.day16)
(require '[clojure.string :as str])

(def input "00537390040124EB240B3EDD36B68014D4C9ECCCE7BDA54E62522A300525813003560004223BC3F834200CC108710E98031C94C8B4BFFF42398309DDD30EEE00BCE63F03499D665AE57B698F9802F800824DB0CE1CC23100323610069D8010ECD4A5CE5B326098419C319AA2FCC44C0004B79DADB1EB48CE5EB7B2F4A42D9DF0AA74E66468C0139341F005A7BBEA5CA65F3976200D4BC01091A7E155991A7E155B9B4830056C01593829CC1FCD16C5C2011A340129496A7EFB3CA4B53F7D92675A947AB8A016CD631BE15CD5A17CB3CEF236DBAC93C4F4A735385E401804AA86802D291ED19A523DA310006832F07C97F57BC4C9BBD0764EE88800A54D5FB3E60267B8ED1C26AB4AAC0009D8400854138450C4C018855056109803D11E224112004DE4DB616C493005E461BBDC8A80350000432204248EA200F4148FD06C804EE1006618419896200FC1884F0A00010A8B315A129009256009CFE61DBE48A7F30EDF24F31FCE677A9FB018F6005E500163E600508012404A72801A4040688010A00418012002D51009FAA0051801CC01959801AC00F520027A20074EC1CE6400802A9A004A67C3E5EA0D3D5FAD3801118E75C0C00A97663004F0017B9BD8CCA4E2A7030C0179C6799555005E5CEA55BC8025F8352A4B2EC92ADF244128C44014649F52BC01793499EA4CBD402697BEBD18D713D35C9344E92CB67D7DFF05A60086001610E21A4DD67EED60A8402415802400087C108DB068001088670CA0DCC2E10056B282D6009CFC719DB0CD3980026F3EEF07A29900957801AB8803310A0943200042E3646789F37E33700BE7C527EECD13266505C95A50F0C017B004272DCE573FBB9CE5B9CAE7F77097EC830401382B105C0189C1D92E9CCE7F758B91802560084D06CC7DD679BC8048AF00400010884F18209080310FE0D47C94AA00")

(def hex-to-binary
  {\0 [\0 \0 \0 \0]
   \1 [\0 \0 \0 \1]
   \2 [\0 \0 \1 \0]
   \3 [\0 \0 \1 \1]
   \4 [\0 \1 \0 \0]
   \5 [\0 \1 \0 \1]
   \6 [\0 \1 \1 \0]
   \7 [\0 \1 \1 \1]
   \8 [\1 \0 \0 \0]
   \9 [\1 \0 \0 \1]
   \A [\1 \0 \1 \0]
   \B [\1 \0 \1 \1]
   \C [\1 \1 \0 \0]
   \D [\1 \1 \0 \1]
   \E [\1 \1 \1 \0]
   \F [\1 \1 \1 \1]})

(def parse-packet)

(def binary-to-hex (reduce-kv (fn [res k v] (assoc res v k)) {} hex-to-binary))

(defn partition-packet [packet lengths]
  (loop [packet packet lengths lengths res []]
    (if (empty? lengths)
      res
      (recur (drop (first lengths) packet)
             (rest lengths)
             (conj res (take (first lengths) packet))))))

(defn binary-to-decimal [bin]
  (Integer/parseInt
     (apply str bin)
     2))

(defn parse-literal [packet]
  (let [[pversion ptype] (partition-packet packet [3 3])
        number-data (drop 6 packet)
        [payload remaining] (loop [data number-data res (list)]
                              (let [[flag numbits] (partition-packet data [1 4])
                                    new-res (concat res numbits)
                                    remaining (drop 5 data)]
                                (if (= [\1] flag)
                                  (recur remaining new-res)
                                  [new-res remaining])))]
    {:payload payload :version pversion :remaining remaining :type ptype}))

(defn parse-length-type-zero [packet]
  (let [[pversion ptype lid len] (partition-packet packet [3 3 1 15])
        subpackets-size (binary-to-decimal len)
        subpackets-data (drop 22 packet)
        payload (loop [data subpackets-data res []]
                  (let [consumed (- (count subpackets-data)
                                    (count data))]
                    (if (<= subpackets-size consumed)
                      {:payload res :remaining data :version pversion :type ptype}
                      (let [payload (parse-packet data)]
                        (recur (:remaining payload)
                               (into [] (conj res payload)))))))]
    payload))

(defn parse-length-type-one [packet]
  (let [[pversion ptype lid len] (partition-packet packet [3 3 1 11])
        num-subpackets (binary-to-decimal len)
        data (drop 18 packet) ;; drop headers
        payload (loop [data data i num-subpackets res []]
                  (if (zero? i)
                    {:payload res :remaining data :version pversion :type ptype}
                    (let [payload (parse-packet data)]
                      (recur (:remaining payload)
                             (dec i)
                             (into [] (conj res payload))))))]
    payload))

(defn parse-operator [packet]
  (let [[pversion ptype lid] (partition-packet packet [3 3 1])
        payload (case lid
                  [\1] (parse-length-type-one packet)
                  [\0] (parse-length-type-zero packet)
                  :else nil
                  )]
    payload))

(defn parse-packet [packet]
  (let [[pversion ptype] (partition-packet packet [3 3])
        payload (cond
                  (= ptype [\1 \0 \0]) (parse-literal packet)
                  :else (parse-operator packet))]
    payload))

(defn parse-hex-packet [hex-packet]
  (let [packet (flatten (map hex-to-binary hex-packet))]
    (parse-packet packet)))

(defn get-versions [data]
  (cond
    (not (some? data)) []
    (and (or (list? data)
             (vector? data))
         (every? map? data)) (apply concat (for [d data] (get-versions d)))
    (map? data) (let [other (doall (get-versions (:payload data)))
                      res (conj other (get data :version))]
                  res)
    :else []))

(defn create-s-exp [tree]
  (let [type-id (:type tree)
        payload (:payload tree)
        type-dec (binary-to-decimal type-id)
        args (if (every? map? payload)
               (map create-s-exp payload)
               payload)
        oper (case type-dec
               4   (BigInteger. (apply str payload) 2)
               0   (conj args '+)
               1   (conj args '*)
               2   (conj args 'min)
               3   (conj args 'max)
               5   (list 'if (conj args '>) 1 0)
               6   (list 'if (conj args '<) 1 0)
               7   (list 'if (conj args '=) 1 0)
               )]
    oper))

(defn solve []
  (let [tree (parse-hex-packet input)
        versions (get-versions tree)
        p1 (reduce + (map binary-to-decimal versions))

        s-exp (create-s-exp tree)
        result (eval s-exp)]
    {:part1 p1 :part2 result}))

(defn status [] "*")
