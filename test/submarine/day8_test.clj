(ns submarine.day8-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day8 :as d8]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       s/split-lines
       (map #(s/split % #"\|"))
       (map (fn [[i o]] [(s/split (s/trim i) #"\s")
                        (s/split (s/trim o) #"\s")]))
       (into [])))

(deftest test-count-uniques
  (is (= (reduce + (map #(d8/count-uniques %) (parse "test1-day8.txt")))
         26)))

(deftest test-output-value
  (is (= (reduce + (map #(d8/output-value %) (parse "test1-day8.txt")))
         61229)))
