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
  (let [pf {"日本" {0 [0 3 5], 1 [1]}, "本酒" {0 [0 5 7]}}
        f  #(get pf %)
        n  4]
    (let [s0 (score/tf-idf-score f n ["日本" "本酒"] 0)
          s1 (score/tf-idf-score f n ["日本" "本酒"] 1)]
      (is (> s0 s1))
      (is (pos? s1)))))

(deftest rank-orders-by-score-desc
  ;; 同じ tf 構成で、TF の多い文書が上位、同点は doc-id 昇順
  (let [pf {"あ" {0 [0], 1 [0 1 2], 2 [2]}}
        f  #(get pf %)]
    (is (= [1 0 2] (map :doc-id (score/rank f 5 "あ" [0 1 2]))))))

(deftest bm25-idf-non-negative-and-monotone
  ;; 希少なほど大きい。DF=N でも負にならない（TF-IDF の log(N/DF)=0 と対比）
  (is (< (score/bm25-idf 100 50) (score/bm25-idf 100 2)))
  (is (pos? (score/bm25-idf 3 3))))

(deftest bm25-tf-saturates
  ;; tf を増やすと増分が単調減少し、(k1+1) に漸近する
  (let [s (fn [tf] (score/bm25-term-score 1.0 tf 1 1.0 1.2 0.0))]
    (is (= 1.0 (s 1)))
    (is (< (- (s 3) (s 2)) (- (s 2) (s 1))))
    (is (< (s 100) 2.2))))

(deftest bm25-length-normalization
  ;; 同じ tf でも、長い文書(dl>avgdl)はスコアが低い
  (let [short (score/bm25-term-score 1.0 1 2 4.0 1.2 0.75)
        long  (score/bm25-term-score 1.0 1 8 4.0 1.2 0.75)]
    (is (> short long))))

(deftest bm25-rank-flips-tfidf-for-long-doc
  ;; 短い高純度文書 vs 長い低純度文書。TF-IDF は長い方(tf大)を上位に、
  ;; BM25(b>0) は短い方を上位に逆転させる。
  (let [pf {"あ" {0 [1], 1 [0 2]}}
        f  #(get pf %)
        dl {0 2, 1 10}
        dlf #(get dl %)
        n 3, avgdl 4.333]
    ;; TF-IDF: tf の多い doc1 が上
    (is (= [1 0] (map :doc-id (score/rank f n "あ" [0 1]))))
    ;; BM25(b=0): 文書長無視 → TF-IDF 同様 doc1 が上
    (is (= [1 0] (map :doc-id (score/bm25-rank f n dlf avgdl "あ" [0 1] :b 0.0))))
    ;; BM25(b=0.75): 文書長正規化で doc0 に逆転
    (is (= [0 1] (map :doc-id (score/bm25-rank f n dlf avgdl "あ" [0 1] :b 0.75))))))
