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
  (let [pf {"日本" [0 2] "本酒" [0]}
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
