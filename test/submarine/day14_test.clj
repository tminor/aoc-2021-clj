(ns submarine.day14-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day14 :as d14]))

(defn parse-rules [lines]
  (->> (s/split-lines lines)
       (map #(s/join (s/split % #" -> ")))
       (map #(char-array %))
       (map #(vector [(nth % 0) (nth % 1)] [(nth % 0) (nth % 2) (nth % 1)]))
       (into {})))

(defn parse [f]
  (-> f
      io/resource
      slurp
      s/trim
      (s/split #"\n\n")
      ((fn [[template rules]]
         [template (parse-rules rules)]))))

(deftest test-apply-rules
  (let [res (last (take 10 (d14/gen-polymer-char-frequencies (parse "test0-day14.txt"))))
        sorted (sort (vals res))
        most (last sorted)
        least (first sorted)]
    (is (= (- most least) 1588)))
  (let [res (last (take 40 (d14/gen-polymer-char-frequencies (parse "test0-day14.txt"))))
        sorted (sort (vals res))
        most (last sorted)
        least (first sorted)]
    (is (= (- most least) 2188189693529))))
