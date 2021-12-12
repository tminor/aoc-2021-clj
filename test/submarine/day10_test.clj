(ns submarine.day10-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day10 :as d10]
            [clojure.zip :as z]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/split-lines
       (map char-array)
       (map (fn [l] (map #(vector (hash-map :char % :matched? false)) l)))
       (map #(into [] %))
       (map #(z/zipper vector? seq (fn [_ c] c) %))))

(deftest test-syntax-error-score
  (is (= (d10/syntax-error-score (parse "test0-day10.txt"))
         26397)))

(deftest test-syntax-completion-score
  (is (= (d10/syntax-completion-score (parse "test0-day10.txt"))
         288957)))
