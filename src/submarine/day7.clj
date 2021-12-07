(ns submarine.day7)

(defn calculate-fuel-cost [position-freqs pos]
  (reduce + (map (fn [[p f]] (* (Math/abs (- p pos)) f)) position-freqs)))

(defn calculate-real-fuel-cost [position-freqs pos]
  (reduce + (map (fn [[p f]] (* (reduce + (range 0 (inc (Math/abs (- p pos))))) f))
                 position-freqs)))

(defn position-costs [positions calc-func]
  (let [ps (range (first (first positions)) (inc (first (last positions))))]
    (map #(calc-func positions %) ps)))
