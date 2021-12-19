(ns submarine.day15-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day15 :as d15]
            [clojure.core.matrix :as m]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       s/split-lines
       (map #(into [] (map (fn [i] (Integer. i)) (s/split % #""))))
       (into [])
       to-array-2d))

(deftest test-expand-matrix
  (is (every? zero? (flatten (m/sub (parse "test1-day15.txt")
                                    (d15/expand-matrix (parse "test0-day15.txt")))))))

(deftest test-dijkstra
  (is (= (get (d15/dijkstra (parse "test0-day15.txt") [0 0]) [9 9])
         40))
  (is (= (get (d15/dijkstra (parse "test1-day15.txt") [0 0]) [49 49])
         315))
  (is (= (get (d15/dijkstra (d15/expand-matrix (parse "test0-day15.txt")) [0 0]) [49 49])
         315)))
