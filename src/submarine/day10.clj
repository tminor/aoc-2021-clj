(ns submarine.day10
  (:require [clojure.zip :as z]))

(def close->open {\) \(, \] \[, \} \{, \> \<,})
(def open->close  {\( \), \[ \], \{ \}, \< \>})
(def corrupt-scores {\) 3, \] 57, \} 1197, \> 25137})
(def incomplete-scores {\) 1, \] 2, \} 3, \> 4})

(defn check-syntax
  ([st]
   (check-syntax (z/down st) '()))
  ([st stack]
   (let [c (first st)]
     (cond
       (some #(= c %) (vals close->open)) (check-syntax (z/right st) (cons c stack))
       (= (first stack) (get close->open c)) (check-syntax (z/right st) (rest stack))
       :else (if (nil? c) stack c)))))

(defn syntax-error-score [lines]
  (apply + (map  #(get corrupt-scores %) (filter #(char? %) (map check-syntax lines)))))

(defn syntax-completion-score [lines]
  (->> lines
       (map check-syntax)
       (filter #(seq? %))
       (map (fn [l] (map #(get incomplete-scores (get open->close %)) l)))
       (map (fn [l] (reduce #(+ (* %1 5) %2) 0 l)))
       sort
       ((fn [scores] (nth scores (quot (count scores) 2))))))
