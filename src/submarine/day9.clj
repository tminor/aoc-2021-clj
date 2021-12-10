(ns submarine.day9)

(defn add-edges [cave-map]
  (->> cave-map
       (map #(concat [9] % [9]))
       ((fn [lines]
          (let [edge (repeat (count (first lines)) 9)]
            (concat [edge] lines [edge]))))
       (into [])))

(defn add-visited [cave-map]
  (map (fn [row] (map #(vector % false) row)) cave-map))

(defn adjacents [cave-map [x y]]
  (let [edged-cm (add-edges cave-map)
        row (nth edged-cm (inc y))
        prev-row (nth edged-cm y)
        next-row (nth edged-cm (+ y 2))]
    [[(nth row x) (nth row (inc x)) (nth row (+ x 2))]
     [(nth prev-row (inc x)) (nth row (inc x)) (nth next-row (inc x))]]))

(defn cave-map->neighbors [cave-map]
  (apply concat
         (map-indexed (fn [y r]
                        (map-indexed (fn [x _]
                                       (adjacents cave-map [x y])) r))
                      cave-map)))

(defn low-point? [[ud lr]]
  (and (and (< (nth ud 1) (nth ud 0))
            (< (nth ud 1) (nth ud 2)))
       (and (< (nth lr 1) (nth lr 0))
            (< (nth lr 1) (nth lr 2)))))

(defn low-points [cave-map]
  (map #(second (first %))
       (filter low-point? (cave-map->neighbors cave-map))))

(defn take-basin-direction [d [x y] cave-map]
  (let [[beg-x beg-y] (condp = d
                        :up [x (dec y)]
                        :left [(dec x) y]
                        :right [(inc x) y]
                        :down [x (inc y)])]
    (take-while #(and (< beg-x (count (first cave-map)))
                      (< beg-y (count cave-map))
                      (let [v (nth (nth cave-map (if (or (= d :up) (= d :down)) % y))
                                   (if (or (= d :left) (= d :right)) % x))]
                        (and (< (first v) 9)
                             (not (second v)))))
                (iterate (if (or (= d :up) (= d :left)) dec inc)
                         (if (or (= d :up) (= d :down)) beg-y beg-x)))))

(defn visit [cm [x y]]
  (let [cm (into [] cm)
        row (into [] (nth cm y))
        node (nth row x)
        visited [(first node) true]]
    `[~@(subvec cm 0 y)
      ~`[~@(subvec row 0 x) ~visited ~@(subvec row (inc x))]
      ~@(subvec cm (inc y))]))

(defn walk-basin-direction
  ([d [x y] cm]
   (walk-basin-direction d [x y] cm false))
  ([d [x y] cm visit?]
   (cond->> cm
     true (take-basin-direction d [x y])
     true (reduce #(conj %1 [(if (or (= d :left) (= d :right)) %2 x)
                             (if (or (= d :up) (= d :down)) %2 y)])
                  [])
     true (into [])
     visit? (reduce visit cm))))

(defn fan-out [cm nodes]
  (if-let [node (first nodes)]
    (let [walked `[~@(walk-basin-direction :up node cm)
                   ~@(walk-basin-direction :left node cm)
                   ~@(walk-basin-direction :right node cm)
                   ~@(walk-basin-direction :down node cm)]
          cm (as-> cm $
               (walk-basin-direction :up node $ true)
               (walk-basin-direction :left node $ true)
               (walk-basin-direction :right node $ true)
               (walk-basin-direction :down node $ true))
          visited (->> walked flatten (partition 2 2) distinct)]
      (fan-out cm (concat (rest nodes) visited)))
    cm))

(defn basin-size [cm [x y]]
  (let [cm (add-visited (add-edges cm))
        direction (->> {:up [(inc x) y], :left [x (inc y)],
                        :right [(+ x 2) (inc y)], :down [(inc x) (+ y 2)]}
                       (map (fn [[k [x y]]] [k (nth (nth cm y) x)]))
                       (into {})
                       (filter #(< (first (second %1)) 9))
                       first
                       first)
        nodes (walk-basin-direction direction [(inc x) (inc y)] cm)]
    (->> (fan-out cm nodes)
         (map (fn [row] (map second row)))
         (apply concat)
         (filter true?)
         count)))
