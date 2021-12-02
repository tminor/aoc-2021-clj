(ns submarine.day1-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day1 :as d1]))

(defn parse [f]
  (map #(Integer. %) (s/split-lines (slurp (io/resource f)))))

(deftest test-count-increases
  (is (= (d1/count-increases (parse "test0-day1.txt"))
         7)
      "Part 1"))

(deftest test-count-sliding-window-increases
  (is (= (d1/count-sliding-window-increases (parse "test0-day1.txt"))
         5)
      "Part 2"))
