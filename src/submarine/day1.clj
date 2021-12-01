(ns submarine.day1)

(defn count-increases [input]
  (reduce (fn [memo [j k]] (if (> k j) (inc memo) memo)) 0 (partition 2 1 input)))

(defn count-sliding-window-increases [input]
  (count-increases (map #(apply + %) (partition 3 1 input))))
