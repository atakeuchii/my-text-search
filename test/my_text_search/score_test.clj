(ns my-text-search.score-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.score :as score]))

(deftest idf-basic
  ;; 全文書に出る語(DF=N)は IDF=0、希少なほど大きい
  (is (= 0.0 (score/idf 5 5)))
  (is (< (score/idf 5 4) (score/idf 5 1)))
  (is (= 0.0 (score/idf 5 0))))

(deftest tf-idf-score-uses-tf-and-idf
  ;; posting-fn を map で差し替え。term "日本": DF=2, term "本酒": DF=1
  (let [pf {"日本" {0 3, 1 1}, "本酒" {0 3}}
        f  #(get pf %)
        n  4]
    (let [s0 (score/tf-idf-score f n ["日本" "本酒"] 0)
          s1 (score/tf-idf-score f n ["日本" "本酒"] 1)]
      (is (> s0 s1))
      (is (pos? s1)))))

(deftest rank-orders-by-score-desc
  ;; 同じ tf 構成で、TF の多い文書が上位、同点は doc-id 昇順
  (let [pf {"あ" {0 1, 1 3, 2 1}}
        f  #(get pf %)]
    (is (= [1 0 2] (map :doc-id (score/rank f 5 "あ" [0 1 2]))))))
