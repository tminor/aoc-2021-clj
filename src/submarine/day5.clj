(ns submarine.day5)

(defn vertical? [[[x1 _] [x2 _]]] (= x1 x2))

(defn horizontal? [[[_ y1] [_ y2]]] (= y1 y2))

(defn diagonal? [[[x1 y1] [x2 y2]]]
  (not (or (vertical? [[x1 y1] [x2 y2]])
           (horizontal? [[x1 y1] [x2 y2]]))))

(defn max-x [segments]
  (last (sort (map (fn [[[x1 _] [x2 _]]] (last (sort [x1 x2]))) segments))))

(defn max-y [segments]
  (last (sort (map (fn [[[_ y1] [_ y2]]] (last (sort [y1 y2]))) segments))))

(defn make-empty-plane [max-x max-y]
  (into [] (repeat (inc max-y) (apply vector (map #(- % %) (range 0 (inc max-x)))))))

(defn increment-nth [v n]
  `[~@(subvec v 0 n) ~(inc (nth v n)) ~@(subvec v (inc n))])

(defn replace-nth [v n r]
  `[~@(subvec v 0 n) ~r ~@(subvec v (inc n))])

(defn inc-points [row-or-col points]
  (if (seq points)
    (inc-points (increment-nth row-or-col (first points))
                (rest points))
    row-or-col))

(defn rotate [plane]
  (apply vector
         (apply (partial map (fn [& cols-or-rows] (apply vector cols-or-rows)))
                plane)))

(defn diagonal-points [[[x1 y1] [x2 y2]]]
  (let [[x-beg x-end] (sort [x1 x2])
        [y-beg y-end] (sort [y1 y2])
        x-points (if (> x1 x2)
                   (reverse (range x-beg (inc x-end)))
                   (range x-beg (inc x-end)))
        y-points (if (> y1 y2)
                   (reverse (range y-beg (inc y-end)))
                   (range y-beg (inc y-end)))]
    (map vector x-points y-points)))

(defn plot-point [[x y] plane]
  (replace-nth plane y (replace-nth (nth plane y) x (inc (nth (nth plane y) x)))))

(defn plot-diagonal
  ([[[x1 y1] [x2 y2]] plane]
   (plot-diagonal [[x1 y1] [x2 y2]] plane (diagonal-points [[x1 y1] [x2 y2]])))
  ([_ plane points]
   (if (seq points)
     (plot-diagonal nil (plot-point (first points) plane) (rest points))
     plane)))

(defn plot-segment [[[x1 y1] [x2 y2]] plane]
  (cond
    (vertical? [[x1 y1] [x2 y2]])
    (let [rotated (rotate plane)
          col (nth rotated x1)
          points (range y1 (inc y2))]
      (rotate (replace-nth rotated x1 (inc-points col points))))
    (horizontal? [[x1 y1] [x2 y2]])
    (let [row (nth plane y1)
          points (range x1 (inc x2))]
      (replace-nth plane y1 (inc-points row points)))
    (diagonal? [[x1 y1] [x2 y2]])
    (plot-diagonal [[x1 y1] [x2 y2]] plane)))

(defn plot-segments
  ([segments]
   (plot-segments segments (make-empty-plane (max-x segments) (max-y segments))))
  ([segments plane]
   (if (seq segments)
     (plot-segments (rest segments)
                    (plot-segment (sort (first segments)) plane))
     plane)))

(defn count-intersections [plane]
  (count (apply concat (map (partial filter #(< 1 %)) plane))))
