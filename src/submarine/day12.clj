(ns submarine.day12
  (:require [clojure.string :as s])
  (:require [clojure.pprint :as pp]))

(defrecord cave [name size links])

(defmethod pp/simple-dispatch cave [obj]
  (let [links (into {} (map (fn [[d n]] (if n [d (str n)] [d nil]))
                            (deref (:links obj))))]
    (pp/with-pprint-dispatch print
      (pp/pprint (assoc obj :links links)))))

(defn prune-invalid-nodes
  ([nodes]
   (prune-invalid-nodes nodes (hash nodes)))
  ([nodes hashed]
   (doall (doseq [n nodes]
            (let [links (vals (deref (:links n)))]
              (when (and (= (count links) 1) (= (:size (first links)) :small))
                (doall (doseq [l links]
                         (swap! (:links l) #(dissoc % (:name n)))))))))
   (when (not (= hashed (hash nodes)))
     (recur nodes (hash nodes)))))

(defn lines->cave-links [lines]
  (let [caves (->> lines
                   (map #(s/split % #"-"))
                   flatten
                   distinct)]
    (into {} (map (fn [c]
                    (let [links (filter #(or (s/includes? % (str "-" c))
                                             (s/includes? % (str c "-")))
                                        lines)
                          others (map #(first (filter (comp not (partial = c))
                                                      (s/split % #"-"))) links)]
                      [(keyword c) (into [] (map keyword others))]))
                  caves))))

(defn links->graph
  ([links]
   (links->graph links true))
  ([links prune?]
   (let [caves (map (fn [[c ls]]
                      (let [s (cond
                                (re-matches #"[A-Z]+" (name c)) :big
                                (re-matches #"[a-z]+" (name c)) :small)]
                        (->cave c s (atom (into {} (map #(vector % nil) ls))))))
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
     (when prune? (prune-invalid-nodes caves))
     (first (filter #(= :start (:name %)) caves)))))

(defn valid-path? [path]
  (let [freqs (frequencies path)]
    (reduce (fn [memo [v freq]]
              (and memo (not (and (= (:size v) :small) (< 1 freq)))))
            true freqs)))

(defn valid-path-single-small-twice? [path]
  (let [freqs (frequencies (filter #(= (:size %) :small) path))
        twice-visits (keys (filter (fn [[_ f]] (= f 2)) freqs))
        others (remove (fn [[n _]] (= n (first twice-visits))) freqs)]
    (and (<= (count twice-visits) 1)
         (<= (count (filter #(= (:name %) :start) path)) 1)
         (<= (count (filter #(= (:name %) :end) path)) 1)
         (reduce (fn [memo [v freq]]
                   (and memo (not (and (= (:size v) :small) (< 1 freq)))))
                 true others))))

(defn new-candidates [candidate mode]
  (let [cand (into [] candidate)
        links (into [] (vals (deref (:links (last candidate)))))
        filter-fn (if (= mode 1) valid-path? valid-path-single-small-twice?)]
    (filter filter-fn
            (map #(conj cand %) links))))

(defn complete? [path]
  (= (:name (last path)) :end))

(defn build-paths
  ([start]
   (build-paths start 1))
  ([start mode]
   (let [candidates (map #(vector start %) (vals (deref (:links start))))]
     (build-paths candidates mode [])))
  ([candidates mode complete]
   (if (not (seq candidates))
     complete
     (let [new-paths (apply concat (map #(new-candidates % mode) candidates))
           completed (filter complete? new-paths)
           new-candidates (filter (comp not complete?) new-paths)]
       (recur new-candidates mode (concat complete completed))))))
