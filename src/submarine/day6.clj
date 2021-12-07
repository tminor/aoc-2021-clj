(ns submarine.day6)

(def fish-counter
  (memoize
   (fn [timer days]
     (if (not (pos? (- days timer)))
       1
       (+ (fish-counter 9 (- days timer))
          (fish-counter 7 (- days timer)))))))

(defn count-fish [timers days]
  (reduce + (map #(fish-counter % days) timers)))
