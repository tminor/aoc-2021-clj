(ns submarine.day9-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day9 :as d9]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       s/split-lines
       (map #(map (fn [i] (Integer. i)) (s/split % #"")))))

(deftest test-low-points
  (is (= (reduce + (map inc (d9/low-points (parse "test0-day9.txt"))))
         15)))

(deftest test-basin-size
  (let [cm (parse "test0-day9.txt")
        coords (apply concat (map-indexed (fn [y row]
                                            (map-indexed (fn [x _] [x y]) row))
                                          cm))
        low-points (filter #(d9/low-point? (d9/adjacents cm %)) coords)]
    (is (= (->> low-points
                (map #(d9/basin-size cm %))
                sort
                reverse
                (take 3)
                (apply *))
           1134)))
  (is (= (d9/basin-size (parse "test1-day9.txt") [0 0])
         12)))
