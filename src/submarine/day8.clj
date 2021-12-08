(ns submarine.day8)

(def encoding
  {"467889" 0, "89" 1, "47788" 2, "77889" 3, "6789" 4,
   "67789" 5, "467789" 6, "889" 7, "4677889" 8, "677889" 9,
   0 "467889", 1 "89", 2 "47788", 3 "77889", 4 "6789",
   5 "67789", 6 "467789", 7 "889", 8 "4677889", 9 "677889"})

(defn decoder [segments]
  (let [cfs (frequencies (apply str segments))]
    (into {}
          (map (fn [s]
                 [(sort s) (get encoding (->> (map #(get cfs %) s) sort (apply str)))])
               segments))))

(defn count-uniques [[input output]]
  (let [d (decoder input)
        nums (map #(get d (sort %)) output)]
    (count (filter #(or (= % 1) (= % 4) (= % 7) (= % 8))
                   nums))))

(defn output-value [[input output]]
  (let [d (decoder input)]
    (Integer. (apply str (map #(get d (sort %)) output)))))
