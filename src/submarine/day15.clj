(ns submarine.day15)

(defn graph->nodes [graph]
  (into [] (for [[x row] (map-indexed vector graph)
                 [y weight] (map-indexed vector row)]
             [[x y] weight])))

(defn inc-matrix
  ([matrix]
   (cons matrix (inc-matrix matrix true)))
  ([matrix _]
   (let [height (count matrix)
         width (count (first matrix))
         incremented (for [row (map identity matrix), cell (map identity row)]
                       (if (< 9 (inc cell))
                         1 (inc cell)))
         new-matrix (->> incremented (partition width height) to-array-2d)]
     (lazy-seq (cons new-matrix (inc-matrix new-matrix true))))))

(defn expand-matrix [matrix]
  (let [top-row (take 5 (inc-matrix matrix))
        columns (map #(take 5 (inc-matrix %)) top-row)
        blocks (apply vector
                      (apply (partial map (fn [& cols-or-rows] (apply vector cols-or-rows)))
                             columns))]
    (to-array-2d
     (apply concat
            (map (fn [row]
                   (map (fn [block-row]
                          (apply concat
                                 (map (fn [block]
                                        (nth block block-row))
                                      (nth blocks row))))
                        (range 0 (count matrix))))
                 (range 0 5))))))

(defn node-neighbors [[x y] [max-x max-y]]
  (->> [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]]
       (filter (fn [[x y]] (and (not (neg? x)) (not (neg? y))
                               (<= x max-x) (<= y max-y))))
       (into [])))

(defn update-dist-and-prev [dist prev node-loc graph neighbors]
  (reduce (fn [[old-dist old-prev] [x y]]
            (let [alt-weight (+ (get old-dist node-loc)
                                (aget graph x y))]
              (if (< alt-weight (get old-dist [x y]))
                [(assoc old-dist [x y] alt-weight)
                 (assoc old-prev [x y] node-loc)]
                [old-dist old-prev])))
          [dist prev]
          neighbors))

(defn next-node [dist queue]
  (first
   (into (sorted-map-by (fn [k1 k2]
                          (compare (get dist k1)
                                   (get dist k2))))
         queue)))

(defn dijkstra
  ([graph source]
   (let [nodes (graph->nodes graph)
         dist (assoc (into {}
                           (map #(vector (first %) ##Inf) nodes)) source 0)
         queue (into (sorted-map-by (fn [k1 k2]
                                      (compare [(get dist k1) k1]
                                               [(get dist k2) k2])))
                     nodes)
         prev (into {} (map #(vector (first %) nil) nodes))
         node (first queue)]
     (dijkstra node graph source (dissoc queue (first node)) dist prev)))
  ([[loc _] graph source queue dist prev]
   (if (not (seq queue))
     dist
     (let [neighbors (filter (fn [[x y]] (get queue [x y]))
                             (node-neighbors loc [(dec (count graph))
                                                  (dec (count (first graph)))]))
           [new-dist new-prev] (update-dist-and-prev dist prev loc graph neighbors)
           next-node (first queue)]
       (recur next-node graph source (dissoc queue (first next-node)) new-dist new-prev)))))
