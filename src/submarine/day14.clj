(ns submarine.day14)

(defn rules->rule-char-map [rules]
  (->> rules
       (map (fn [[rule tuple]] [rule (second tuple)]))
       (into {})))

(defn rules->rule-pairs-map [rules]
  (->> rules
       (map (fn [[rule tuple]] [rule (partition 2 1 tuple)]))
       (into {})))

(defn chars->char-freq-map [rules template]
  (merge (->> rules (into []) flatten distinct (map #(vector % 0))
              (into {}))
         (frequencies template)))

(defn pairs->pair-freq-map [rules template]
  (merge (->> rules keys distinct (map #(vector % 0))
              (into {}))
         (frequencies (partition 2 1 template))))

(defn apply-rules [unapplied-rules rules rule->char rule->pairs char->freq pair->freq]
  (let [[rule times] (first unapplied-rules)
        c (get rule->char rule)
        char-freq (assoc char->freq c (+ (get char->freq c) times))
        [p1 p2] (get rule->pairs rule)
        ;; There must be a better way...
        pair-freq1 (assoc pair->freq rule (- (get pair->freq rule) times))
        pair-freq2 (assoc pair-freq1 p1 (+ (get pair-freq1 p1) times))
        pair-freq (assoc pair-freq2 p2 (+ (get pair-freq2 p2) times))]
    (if (seq (dissoc unapplied-rules rule))
      (recur (dissoc unapplied-rules rule) rules rule->char rule->pairs char-freq pair-freq)
      [char-freq pair-freq])))

(defn gen-polymer-char-frequencies
  ([[template rules]]
   (let [rule->char  (rules->rule-char-map rules)
         rule->pairs (rules->rule-pairs-map rules)
         char->freq  (chars->char-freq-map rules template)
         pair->freq  (pairs->pair-freq-map rules template)]
     (gen-polymer-char-frequencies rules rule->char rule->pairs char->freq pair->freq)))
  ([rules rule->char rule->pairs char->freq pair->freq]
   (let [unapplied-rules (->> pair->freq
                              (filter (fn [[_ f]] (not (or (zero? f) (neg? f)))))
                              (map (fn [[p f]] [p f]))
                              (into {}))
         [char-freq pair-freq]
         (apply-rules unapplied-rules rules rule->char rule->pairs char->freq pair->freq)]
     (lazy-seq
      (cons char-freq
            (gen-polymer-char-frequencies rules rule->char rule->pairs char-freq pair-freq))))))
