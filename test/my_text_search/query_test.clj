(ns my-text-search.query-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.query :as q]))

(deftest and-postings-test
  (is (= [1 3] (q/and-postings [[0 1 2 3] [1 3 5] [1 3 9]])))
  (is (= [0 2] (q/and-postings [[0 2]])))
  (is (= [] (q/and-postings [[0 1 2] []])))
  (is (= [] (q/and-postings []))))

(deftest or-postings-test
  (is (= [0 1 2 4 5] (q/or-postings [[0 2 4] [1 2 5]])))
  (is (= [1 2 3] (q/or-postings [[1 2] [2 3] [1 3]])))
  (is (= [] (q/or-postings []))))

(deftest search-test
  ;; posting-fn は {文書ID -> TF} を返す
  (let [pf {"日本" (sorted-map 0 1 2 3) "本酒" (sorted-map 0 2)}
        f #(get pf %)]
    (is (= [0] (vec (q/search f "日本酒"))))
    (is (= [0 2] (vec (q/search f "日本酒" :op :or))))
    (is (= [] (vec (q/search f "焼酎"))))
    (is (= [] (vec (q/search f ""))))))

(deftest hydrate-test
  (let [df {0 "日本酒の醸造" 2 "日本語の文法"}]
    (is (= [{:doc-id 0 :text "日本酒の醸造"}
            {:doc-id 2 :text "日本語の文法"}]
           (q/hydrate df [0 2])))
    (is (= [] (q/hydrate df [])))
    (is (= [2 0] (map :doc-id (q/hydrate df [2 0]))))))

(deftest wildcard-expands-and-ors
  (let [wf (fn [prefix]
             (->> {"日本酒"   (sorted-map 0 2)
                   "日本酒造" (sorted-map 1 1)
                   "日本語"   (sorted-map 3 1)
                   "焼酎"     (sorted-map 4 1)}
                  (filter (fn [[w _]] (.startsWith ^String w prefix)))
                  (sort-by first)))]
    (is (= ["日本酒" "日本酒造"] (map first (q/wildcard-terms wf "日本酒"))))
    (is (= [0 1] (vec (q/wildcard-search wf "日本酒"))))
    (is (= [0 1 3] (vec (q/wildcard-search wf "日本"))))
    (is (= 1 (count (q/wildcard-terms wf "日本" :max-terms 1))))
    (is (= [] (vec (q/wildcard-search wf "存在しない"))))))

(deftest wildcard-query-detection
  (is (true?  (q/wildcard-query? "日本酒*")))
  (is (false? (q/wildcard-query? "日本酒"))))
