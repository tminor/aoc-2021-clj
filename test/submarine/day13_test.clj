(ns submarine.day13-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day13 :as d13]))

(defn parse-instruction [i]
  (let [split (s/split i #"=")
        axis (-> split first last str keyword)
        value (-> split second Integer/parseInt)]
    [(if (= axis :x) value 0)
     (if (= axis :y) value 0)]))

(defn parse-coordinate [coordinate]
  (let [[x y] (s/split coordinate #",")]
    [(Integer. x) (Integer. y)]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       ((fn [f] (s/split f #"\n\n")))
       ((fn [[coords insts]]
          {:coordinates (into [] (map parse-coordinate (s/split-lines coords)))
           :instructions (into [] (map parse-instruction (s/split-lines insts)))}))))
