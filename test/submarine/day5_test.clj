(ns submarine.day5-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day5 :as d5]))

(defn parse [f]
  (-> f
      io/resource
      slurp
      s/split-lines
      ((partial map #(s/split % #"\s->\s")))
      ((partial map #(vector (apply vector (map (fn [i] (Integer. i)) (s/split (first %) #",")))
                             (apply vector (map (fn [i] (Integer. i)) (s/split (second %) #","))))))
      ((partial apply vector))))

(deftest test-intersection-count
  (is (= (d5/count-intersections (d5/plot-segments (filter #(or (d5/vertical? %) (d5/horizontal? %)) (parse "test0-day5.txt"))))
         5))
  (is (= (d5/count-intersections (d5/plot-segments (parse "test0-day5.txt")))
         12)))
