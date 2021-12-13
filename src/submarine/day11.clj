(ns submarine.day11
  (:require [clojure.pprint :as pp]))

(def opposite-direction {:n :s, :e :w, :s :n, :w :e
                         :ne :sw, :se :nw, :sw :ne, :nw :se})

(defprotocol Neighbor
  (neighbor! [this direction node]))

(defprotocol Flash
  (flash! [this])
  (maybe-flash [this])
  (maybe-reset [this]))

(defrecord Octopus [energy-level neighbors flash-counter loc]
  Neighbor
  (neighbor! [this d n] (assoc this :neighbors (assoc (:neighbors this) d n)))
  Flash
  (maybe-flash [this]
    (when (not (neg? (deref (:energy-level this))))
      (if (= 9 (deref (:energy-level this)))
        (do (swap! (:energy-level this) inc)
            (flash! this))
        (swap! (:energy-level this) inc))))
  (flash! [this]
    (swap! (:flash-counter this) inc)
    (doall (doseq [[_ n] (:neighbors this)]
             (when (and (not (nil? n)) (not (nil? (deref n))))
               (maybe-flash (deref n))))))
  (maybe-reset [this]
    (when (< 9 (deref (:energy-level this)))
      (swap! (:energy-level this) (fn [_] 0)))))

(defmethod pp/simple-dispatch Octopus [obj]
  (let [neighbors (into {} (map (fn [[d n]] (if n [d (str n)] [d nil]))
                                (:neighbors obj)))]
    (pp/with-pprint-dispatch print
      (pp/pprint (assoc obj :neighbors neighbors)))))

(defn add-neighbors [g o [x y]]
  (let [neighbors {:n [x (dec y)], :ne [(inc x) (dec y)],
                   :e [(inc x) y], :se [(inc x) (inc y)],
                   :s [x (inc y)], :sw [(dec x) (inc y)],
                   :w [(dec x) y], :nw [(dec x) (dec y)]}
        height (count g)
        width (count (first g))]
    (last (map (fn [[d [x y]]]
                 (let [node (if (or (<= width x) (<= height y) (neg? x) (neg? y))
                              nil (aget g x y))]
                   (when node
                     (swap! o  #(neighbor! % d node))
                     (swap! node #(neighbor! % (d opposite-direction) o)))
                   o))
               neighbors))))

(defn grid->octopodes [arr]
  (let [neighbors {:n nil, :ne nil, :e nil, :se nil,
                   :s nil, :sw nil, :w nil, :nw nil}
        octopodes (->> (for [[x row] (map-indexed vector arr)
                             [y oct] (map-indexed vector row)]
                         (atom (->Octopus (atom oct) neighbors (atom 0) [x y])))
                       (partition (count (nth arr 0)) (count arr))
                       to-array-2d)]
    (to-array-2d
     (partition (count (nth arr 0)) (count arr)
                (for [[x row] (map-indexed vector octopodes)
                      [y oct] (map-indexed vector row)]
                  (add-neighbors octopodes oct [x y]))))))

(defn tick [grid]
  (doall (for [row (map identity grid), octopus (map identity row)]
           (do (maybe-flash (deref octopus))
               octopus)))
  (doall (for [row (map identity grid), octopus (map identity row)]
           (do (maybe-reset (deref octopus))
               octopus)))
  grid)

(defn visit-neighbor [octopus-atom d]
  (d (:neighbors (deref octopus-atom))))

(defn synchronized? [grid]
  (every? zero?
          (doall (for [row (map identity grid), octopus (map deref row)]
                   (deref (:energy-level octopus))))))

(defn steps-until-synchronized [grid]
  (count (take-while #(not (synchronized? %)) (iterate tick grid))))
