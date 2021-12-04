(ns submarine.day4-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day4 :as d4]))

(defn str->board [s]
  (map #(s/split (s/trim %) #"\s+") (s/split-lines s)))

(defn parse [f]
  (-> f
      io/resource
      slurp
      (s/split #"\n\n")
      ((fn [x] [(s/split (first x) #",")
               (map str->board (rest x))]))))

(deftest test-mark-board
  (is (= (d4/mark-board "20"
                        '(["22" "13" "17" "11" "0"]
                          ["8" "2" "23" "4" "24"]
                          ["21" "9" "14" "16" "7"]
                          ["6" "10" "3" "18" "5"]
                          ["1" "12" "20" "15" "19"]))
         '(["22" "13" "17" "11" "0"]
           ["8" "2" "23" "4" "24"]
           ["21" "9" "14" "16" "7"]
           ["6" "10" "3" "18" "5"]
           ["1" "12" "x" "15" "19"]))
      "A board should be marked with \"x\" when provided a matching number")
  (is (= (d4/mark-board "30"
                        '(["22" "13" "17" "11" "0"]
                          ["8" "2" "23" "4" "24"]
                          ["21" "9" "14" "16" "7"]
                          ["6" "10" "3" "18" "5"]
                          ["1" "12" "20" "15" "19"]))
         '(["22" "13" "17" "11" "0"]
           ["8" "2" "23" "4" "24"]
           ["21" "9" "14" "16" "7"]
           ["6" "10" "3" "18" "5"]
           ["1" "12" "20" "15" "19"]))
      "A board should not be marked with \"x\" when provided a non-matching number"))

(deftest test-check-board
  (is (not (nil? (d4/check-board '(["22" "13" "17" "1" "0"]
                                   ["8" "2" "23" "x" "24"]
                                   ["21" "9" "14" "x" "7"]
                                   ["6" "10" "3" "x" "5"]
                                   ["x" "x" "x" "x" "x"]))))
      "A board with a complete row should return non-nil")
  (is (not (nil? (d4/check-board '(["22" "13" "17" "x" "0"]
                                   ["8" "2" "23" "x" "24"]
                                   ["21" "9" "14" "x" "7"]
                                   ["6" "10" "3" "x" "5"]
                                   ["x" "x" "x" "x" "1"]))))

      "A board with a complete column should return non-nil"))

(deftest test-calculate-winning-score
  (is (= (d4/calculate-board-score '(["x" "x" "x" "x" "x"]
                                     ["10" "16" "15" "x" "19"]
                                     ["18" "8" "x" "26" "20"]
                                     ["22" "x" "13" "6" "x"]
                                     ["x" "x" "12" "3" "x"])
                                   24)
         4512)))

(deftest test-play-bingo
  (is (= (d4/play-bingo (parse "test0-day4.txt") true)
         4512)
      "The first winning board should be chosen if winning is desired")
  (is (= (d4/play-bingo (parse "test0-day4.txt") false)
         1924)
      "The last losing board should be chosen if losing is desired"))
