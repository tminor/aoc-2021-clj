(ns submarine.day6-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day6 :as d6]))

(defn parse [f]
  (-> f
      io/resource
      slurp
      s/trim
      (s/split #",")
      ((partial map #(Integer. %)))))

(deftest test-fish-count
  (is (= (d6/fish-counter 8 8) 1))
  (is (= (d6/fish-counter 1 8) 2)))

(deftest test-count-fish
  (is (= (d6/count-fish (parse "test0-day6.txt") 18)
         26))
  (is (= (d6/count-fish (parse "test0-day6.txt") 80)
         5934))
  ;; Warmup
  (map #(d6/count-fish (parse "test0-day6.txt") %) (take 25 (iterate #(+ 10 %) 100)))
  (is (= (d6/count-fish (parse "test0-day6.txt") 256)
         26984457539)))
