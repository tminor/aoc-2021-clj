(ns submarine.day12-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [submarine.day12 :as d12]))

(defn parse [f]
  (->> f
       io/resource
       slurp
       s/trim
       s/split-lines))

(defn links->nodes [links]
  (let [caves (map (fn [[c ls]]
                     (let [t (cond
                               (re-matches #"[A-Z]+" (name c)) :big
                               (re-matches #"[a-z]+" (name c)) :small)]
                       (d12/->cave c t (atom (into {} (map #(vector % nil) ls))))))
                   links)]
    (doall
     (map (fn [c]
            (let [links (keys (deref (:links c)))
                  others (filter #(some (fn [l] (= l (:name %))) links) caves)]
              (doall
               (map (fn [o]
                      (when (nil? ((:name o) (deref (:links c))))
                        (swap! (:links c) #(assoc % (:name o) o))
                        (swap! (:links o) #(assoc % (:name c) c))))
                    others))
              c))
          caves))
    caves))

(deftest test-prune-invalid-nodes
  (let [nodes (links->nodes (d12/lines->cave-links (parse "test0-day12.txt")))]
    (d12/prune-invalid-nodes nodes)
    (let [b (first (filter #(= (:name %) :b) nodes))]
      (is (= (keys (deref (:links b)))
             [:start :A :end]))))
  (let [nodes (links->nodes (d12/lines->cave-links (parse "test1-day12.txt")))]
    (d12/prune-invalid-nodes nodes)
    (let [b (first (filter #(= (:name %) :he) nodes))]
      (is (= (sort (keys (deref (:links b))))
             (sort [:fs :DX :RW :pj :zg]))))))

(deftest test-valid-path?
  (let [start (d12/->cave :start :small {})
        big (d12/->cave :A :big {})
        small (d12/->cave :c :small {})
        end (d12/->cave :end :small {})]
    (is (d12/valid-path? [start big small big end]))
    (is (not (d12/valid-path? [start small big small end])))))

(deftest test-valid-path-single-small-twice?
  (let [start (d12/->cave :start :small {})
        big (d12/->cave :A :big {})
        small1 (d12/->cave :b :small {})
        small2 (d12/->cave :c :small {})
        end (d12/->cave :end :small {})]
    (is (d12/valid-path-single-small-twice? [start small1 big small2 big small1 end]))
    (is (not (d12/valid-path-single-small-twice? [start small1 big small2 big small1 small2 end])))
    (is (not (d12/valid-path-single-small-twice? [start small1 start big small2 big small2 end])))
    (is (not (d12/valid-path-single-small-twice? [start small1 big small2 end big small2 end])))))

(deftest test-build-paths
  (is (= (count (d12/build-paths (->> (parse "test0-day12.txt")
                                      d12/lines->cave-links
                                      d12/links->graph)))
         10))
  (is (= (count (d12/build-paths (->> (parse "test1-day12.txt")
                                      d12/lines->cave-links
                                      d12/links->graph)))
         226))
  (is (= (count (d12/build-paths (-> (parse "test0-day12.txt")
                                     d12/lines->cave-links
                                     (d12/links->graph false))
                                 2))
         36))
  (is (= (count (d12/build-paths (-> (parse "test1-day12.txt")
                                     d12/lines->cave-links
                                     (d12/links->graph false))
                                 2))
         3509)))
