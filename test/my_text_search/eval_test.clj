(ns my-text-search.eval-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.eval :as eval]))

(deftest precision-recall-f1
  (let [ret [0 1 2 3]
        rel #{0 2 5 6}]
    (is (= 1/2 (rationalize (eval/precision ret rel))))
    (is (= 1/2 (rationalize (eval/recall ret rel))))
    (is (= 0.5 (eval/f1 ret rel))))
  (is (= 1.0 (eval/f1 [0 1 2] #{0 1 2}))))

(deftest boundary-cases
  (is (= 0.0 (eval/precision [] #{0 1})))
  (is (= 0.0 (eval/recall [] #{0 1})))
  (is (= 1.0 (eval/recall [0] #{})))
  (is (= 0.0 (eval/f1 [] #{0 1}))))

(deftest precision-at-k-considers-rank
  (let [ranked [0 3 1 2]
        rel #{0 1}]
    (is (= 1.0 (eval/preciison-at-k ranked rel 1)))
    (is (= 0.5 (eval/preciison-at-k ranked rel 2)))))

(deftest evaluate-aggregates-metrics
  (let [search-fn {"日本酒" [0 1]
                   "日本酒~" [0 1 2]}
        f #(get search-fn %)
        dataset [{:query "日本酒" :relevant #{0 1}}
                 {:query "日本酒~" :relevant #{0 1}}]
        {:keys [per-query mean]} (eval/evaluate f dataset)]
    (is (= 1.0 (:f1 (first per-query))))
    (is (< 0.79 (:f1 (second per-query)) 0.81))
    (is (= 1.0 (:recall mean)))
    (is (< (:precision mean) 1.0))))

(deftest sweep-and-best
  ;; k ごとに retrieved が変わる偽検索
  (let [ret-by-k {0 [0 1], 1 [0 1 3 2], 2 [0 1 3 2]}
        make-fn (fn [k] (fn [_q] (get ret-by-k k)))
        dataset [{:query "日本酒~" :relevant #{0 1 2}}]
        results (eval/sweep make-fn dataset [0 1 2])]
    ;; k=0 は precision 1・recall 低、k=1 は recall 1・precision 低
    (is (= 1.0 (:precision (first results))))
    (is (= 1.0 (:recall (second results))))
    ;; F1 最良は k=1 以上
    (is (>= (:f1 (eval/best-by :f1 results)) 0.85))))
