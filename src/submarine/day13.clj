(ns submarine.day13
  (:require [clojure.string :as s]))

(defn draw-grid [coordinates]
  (let [max-x (apply max (map first coordinates))
        max-y (apply max (map second coordinates))]
    (map (fn [y]
           (println (s/join (map (fn [x] (if (some #(= [x y] %) coordinates) "#" "."))
                                 (range 0 (inc max-x))))))
         (range 0 (inc max-y)))))

(defn translate-coordinate [[x y] [tx ty]]
  (cond
    (and (pos? ty)
         (< ty y)) [x (- y (* 2 (- y ty)))]
    (and (pos? tx)
         (< tx x)) [(- x (* 2 (- x tx))) y]
    :else          [x y]))

(defn translate-coordinates [coordinates [x y]]
  (map #(translate-coordinate % [x y]) coordinates))
