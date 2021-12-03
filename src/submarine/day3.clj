(ns submarine.day3)

(defn calculate-gamma [totals len]
  (reduce (fn [memo i]
            (if (<= (/ len 2) i)
              (str memo "1")
              (str memo "0")))
          "" totals))

(defn calculate-epsilon [gamma]
  (loop [n (- (count gamma) 1)
         g (Integer/parseInt gamma 2)]
    (if (<= 0 n) (recur (dec n) (bit-flip g n)) g)))

(defn power-consumption [input]
  (let [totals (apply map + input)
        len (count input)
        gamma (calculate-gamma totals len)
        epsilon (calculate-epsilon gamma)]
    (* (Integer/parseInt gamma 2) epsilon)))

(defn calculate-rating
  ([input type] (if (= type 'life-support)
                  (* (calculate-rating input 'oxygen 0)
                     (calculate-rating input 'c02-scrubber 0))
                  (calculate-rating input type 0)))
  ([input type pos]
   (let [totals (apply map + input)
         bit (if ((if (= type 'oxygen) <= >) (/ (count input) 2) (nth totals pos)) \1 \0)
         selected (filter #(= (first (str (nth % pos))) bit) input)]
     (if (and (< pos (dec (count totals)))
              (< 1 (count selected)))
       (calculate-rating selected type (inc pos))
       (Integer/parseInt (apply str (first selected)) 2)))))
