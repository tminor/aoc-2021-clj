(ns submarine.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day11 :as d11]
            [clojure.zip :as z]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       s/split-lines
       (map #(into [] (map (fn [i] (Integer. i)) (s/split % #""))))
       (into [])))

(deftest test-equivalent-neighbor-paths
  (let [grid (d11/grid->octopodes (parse "test1-day11.txt"))]
    (is (= (-> (aget grid 0 0) (d11/visit-neighbor :se) (d11/visit-neighbor :ne))
           (-> (aget grid 0 0) (d11/visit-neighbor :e) (d11/visit-neighbor :e))))
    (is (= (-> (aget grid 1 1) (d11/visit-neighbor :n) (d11/visit-neighbor :e))
           (-> (aget grid 1 1) (d11/visit-neighbor :ne))))))

(deftest test-tick
  (let [grid (d11/grid->octopodes (parse "test0-day11.txt"))]
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 5 5))
           '((3 4 5 4 3) (4 0 0 0 4) (5 0 0 0 5) (4 0 0 0 4) (3 4 5 4 3)))
        "Octopodes have correct energy level after 1 step")
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 5 5))
           '((4 5 6 5 4) (5 1 1 1 5) (6 1 1 1 6) (5 1 1 1 5) (4 5 6 5 4)))
        "Octopodes have correct energy level after 1 step"))
  (let [grid (d11/grid->octopodes (parse "test2-day11.txt"))]
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 10 10))
           '((6 5 9 4 2 5 4 3 3 4)
             (3 8 5 6 9 6 5 8 2 2)
             (6 3 7 5 6 6 7 2 8 4)
             (7 2 5 2 4 4 7 2 5 7)
             (7 4 6 8 4 9 6 5 8 9)
             (5 2 7 8 6 3 5 7 5 6)
             (3 2 8 7 9 5 2 8 3 2)
             (7 9 9 3 9 9 2 2 4 5)
             (5 9 5 7 9 5 9 6 6 5)
             (6 3 9 4 8 6 2 6 3 7)))
        "Correct after 1 step")
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 10 10))
           '((8 8 0 7 4 7 6 5 5 5)
             (5 0 8 9 0 8 7 0 5 4)
             (8 5 9 7 8 8 9 6 0 8)
             (8 4 8 5 7 6 9 6 0 0)
             (8 7 0 0 9 0 8 8 0 0)
             (6 6 0 0 0 8 8 9 8 9)
             (6 8 0 0 0 0 5 9 4 3)
             (0 0 0 0 0 0 7 4 5 6)
             (9 0 0 0 0 0 0 8 7 6)
             (8 7 0 0 0 0 6 8 4 8)))
        "Correct after 2 steps")
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 10 10))
           '((0 0 5 0 9 0 0 8 6 6)
             (8 5 0 0 8 0 0 5 7 5)
             (9 9 0 0 0 0 0 0 3 9)
             (9 7 0 0 0 0 0 0 4 1)
             (9 9 3 5 0 8 0 0 6 3)
             (7 7 1 2 3 0 0 0 0 0)
             (7 9 1 1 2 5 0 0 0 9)
             (2 2 1 1 1 3 0 0 0 0)
             (0 4 2 1 1 2 5 0 0 0)
             (0 0 2 1 1 1 9 0 0 0)))
        "Correct after 3 steps")
    (d11/tick grid)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:energy-level col))
                (map deref)
                (partition 10 10))
           '((2 2 6 3 0 3 1 9 7 7)
             (0 9 2 3 0 3 1 6 9 7)
             (0 0 3 2 2 2 1 1 5 0)
             (0 0 4 1 1 1 1 1 6 3)
             (0 0 7 6 1 9 1 1 7 4)
             (0 0 5 3 4 1 1 1 2 2)
             (0 0 4 2 3 6 1 1 2 0)
             (5 5 3 2 2 4 1 1 2 2)
             (1 5 3 2 2 4 7 2 1 1)
             (1 1 3 2 2 3 0 2 1 1)))
        "Correct after 4 steps"))
  (let [grid (d11/grid->octopodes (parse "test2-day11.txt"))
        steps (dotimes [_ 10] (d11/tick grid))]
    (doall steps)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:flash-counter col))
                (map deref)
                (apply +))
           204)))
  (let [grid (d11/grid->octopodes (parse "test2-day11.txt"))
        steps (dotimes [_ 100] (d11/tick grid))]
    (doall steps)
    (is (= (->> (for [row (map identity grid), col (map deref row)]
                  (:flash-counter col))
                (map deref)
                (apply +))
           1656))))

(deftest test-steps-until-synchronized
  (is (= (d11/steps-until-synchronized (d11/grid->octopodes (parse "test2-day11.txt")))
         195)))
