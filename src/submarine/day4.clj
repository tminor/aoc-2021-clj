(ns submarine.day4)

(defn mark-board [n b]
  (map (fn [x]
         (apply concat
                (map (fn [& column]
                       (map #(if (= % n)
                               "x"
                               %)
                            column)) x))) b))

(defn check-board [b]
  (or (seq (filter (partial every? #(= % "x")) b))
      (seq (filter (partial every? #(= % "x")) (apply (partial map list) b)))))

(defn calculate-board-score [b last-n]
  (* last-n
     (apply + (map #(Integer. %)
                   (filter #(not (= % "x")) (flatten b))))))

(defn play-bingo [[nums boards] win?]
  (let [cur-num (first nums)
        marked-boards (map (partial mark-board cur-num) boards)]
    (cond
      ;; Winning is desired and a winning board has been found
      (and win? (first (filter check-board marked-boards)))
      (calculate-board-score (first (filter check-board marked-boards)) (Integer. cur-num))
      ;; Losing desired and one winning board remains
      (and (not win?) (= (count marked-boards) 1) (check-board (first marked-boards)))
      (calculate-board-score (first marked-boards) (Integer. cur-num))
      :else
      (play-bingo (list (rest nums)
                        (if win?
                          marked-boards
                          (filter #(not (check-board %)) marked-boards))) win?))))
