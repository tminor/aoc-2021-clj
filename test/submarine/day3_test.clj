(ns submarine.day3-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day3 :as d3]))

(defn parse [f]
  (-> f
      io/resource
      slurp
      s/split-lines
      ((partial map (fn [l] (map #(Integer. %) (s/split l #"")))))))

(deftest test-power-consumption
  (is (= (d3/power-consumption (parse "test0-day3.txt"))
         198)))


(deftest test-calculate-rating
  (is (= (d3/calculate-rating (parse "test0-day3.txt") 'life-support)
         230)))
