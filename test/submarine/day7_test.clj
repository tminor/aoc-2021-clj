(ns submarine.day7-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day7 :as d7]))

(defn parse [f]
  (-> f
      io/resource
      slurp
      s/trim
      (s/split #",")
      ((partial map #(Integer. %)))
      frequencies
      sort))

(deftest test-calculate-fuel-cost
  (is (= (d7/calculate-fuel-cost (parse "test0-day7.txt") 2)
         37))
  (is (= (d7/calculate-fuel-cost (parse "test0-day7.txt") 1)
         41))
  (is (= (d7/calculate-fuel-cost (parse "test0-day7.txt") 10)
         71)))

(deftest test-calculate-real-fuel-cost
  (is (= (d7/calculate-real-fuel-cost (parse "test0-day7.txt") 2)
         206))
  (is (= (d7/calculate-real-fuel-cost (parse "test0-day7.txt") 5)
         168)))

(deftest test-position-costs
  (is (= (first (sort (d7/position-costs (parse "test0-day7.txt") d7/calculate-fuel-cost)))
         37))
  (is (= (first (sort (d7/position-costs (parse "test0-day7.txt") d7/calculate-real-fuel-cost)))
         168)))
