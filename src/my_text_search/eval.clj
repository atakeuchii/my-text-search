(ns my-text-search.eval
  (:require [clojure.set :as set]))

(defn precision
  [retreived relevant]
  (let [r (set retreived)
        rel (set relevant)]
    (if (empty? r)
      0.0
      (/ (double (count (set/intersection r rel))) (count r)))))

(defn recall
  [retreived relevant]
  (let [r (set retreived)
        rel (set relevant)]
    (if (empty? rel)
      1.0
      (/ (double (count (set/intersection r rel))) (count rel)))))

(defn f1
  [retreived relevant]
  (let [p (precision retreived relevant)
        r (recall retreived relevant)]
    (if (zero? (+ p r))
      0.0
      (/ (* 2 p r) (+ p r)))))

(defn preciison-at-k
  [ranked-ids relevant k]
  (precision (take k ranked-ids) relevant))

(defn evaluate
  [search-fn dataset]
  (let [per (for [{:keys [query relevant]} dataset]
              (let [ret (search-fn query)]
                {:query query
                 :precision (precision ret relevant)
                 :recall (recall ret relevant)
                 :f1 (f1 ret relevant)}))
        mean (fn [key-fn] (if (empty? per)
                            0.0
                            (/ (reduce + (map key-fn per)) (count per))))]
    {:per-query (vec per)
     :mean {:precision (mean :precision)
            :recall (mean :recall)
            :f1 (mean :f1)}}))

(defn sweep
  [make-search-fn dataset params]
  (for [p params]
    (let [{:keys [mean]} (evaluate (make-search-fn p) dataset)]
      (assoc mean :param p))))

(defn best-by
  "指標 key-fn が最大の設定を返す。"
  [key-fn sweep-results]
  (apply max-key key-fn sweep-results))
