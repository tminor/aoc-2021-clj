(ns submarine.day2-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day2 :as d2]))

(defn parse [f]
  (map (fn [l]
         (let [l (s/split l #"\s")
               direction (symbol (first l))
               magnitude (Integer. (second l))]
           (vector direction magnitude)))
       (s/split-lines (slurp (io/resource f)))))

(deftest test-simple-navigate
  (is (= (apply * (d2/navigate false (parse "test0-day2.txt")))
         150)
      "Part 1"))

(deftest test-complex-navigate
  (is (= (apply * (d2/navigate 'with-aim (parse "test0-day2.txt")))
         900)
      "Part 2"))
