(ns submarine.day2)

(defn direction->vector
  [d & prev]
  (let [prev (first prev)]
    (condp = (first d)
      'forward (if prev
                 (vector (second d) (* (last prev) (second d)) (last prev))
                 (vector (second d) 0 0))
      'up      (if prev
                 (vector 0 0 (+ (last prev) (* -1 (second d))))
                 (vector 0 (* -1 (second d)) 0))
      'down    (if prev
                 (vector 0 0 (+ (last prev) (second d)))
                 (vector 0 (second d) 0)))))

(defn navigate
  ([with-aim directions]
   (navigate (map (partial take 2)
                  (if with-aim
                    (rest (reduce #(conj %1 (direction->vector %2 (last %1))) [[0 0 0 ]] directions))
                    (map direction->vector directions)))
             nil
             [0 0]))
  ([directions _ loc]
   (if (seq directions)
     (navigate (rest directions) nil (map + loc (first directions)))
     loc)))
