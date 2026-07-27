(ns my-text-search.index-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.index :as idx]
            [my-text-search.tokenizer :as tok]))

(deftest empty-index-shape
  (let [ix (idx/empty-index)]
    (is (= {} (:docs ix)))
    (is (zero? (:next-id ix)))
    (is (empty? (:postings ix)))
    (is (sorted? (:postings ix)))))

(deftest empty-index-postings-stay-sorted
  (let [p (-> (:postings (idx/empty-index))
              (assoc "本酒" (sorted-map 0 1))
              (assoc "日本" (sorted-map 0 1)))]
    (is (= ["日本" "本酒"] (keys p)))))

(deftest add-one-document
  (let [ix (idx/add-document (idx/empty-index) "日本酒")]
    (is (= 1 (:next-id ix)))
    (is (= "日本酒" (get-in ix [:docs 0])))
    (is (= {0 1} (idx/posting ix "日本")))
    (is (= #{0} (set (keys (idx/posting ix "日本")))))
    (is (= #{0} (set (keys (idx/posting ix "本酒")))))))

(deftest shared-term-collects-doc-ids-sorted
  (let [ix (-> (idx/empty-index)
               (idx/add-document "日本酒")     ; id 0
               (idx/add-document "日本語")     ; id 1
               (idx/add-document "本日"))]     ; id 2
    (is (= (sorted-map 0 1 1 1) (get-in ix [:postings "日本"])))   ; 「日本」は0,1 に各1回
    (is (= [0 1] (keys (get-in ix [:postings "日本"]))))))

(deftest duplicate-term-in-one-doc-counts-tf
  ;; 「あああ」は bigram 「ああ」を2回含むので TF=2
  (let [ix (idx/add-document (idx/empty-index) "あああ")]
    (is (= (sorted-map 0 2) (get-in ix [:postings "ああ"])))))

(deftest reduce-over-corpus
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒" "純米酒" "焼酎"])]
    (is (= 3 (:next-id ix)))
    (is (= 3 (count (:docs ix))))))

(deftest posting-lookup
  (let [ix (-> (idx/empty-index)
               (idx/add-document "日本酒")    ; id 0
               (idx/add-document "日本語"))]   ; id 1
    (is (= (sorted-map 0 1 1 1) (idx/posting ix "日本")))
    (is (= (sorted-map 0 1)     (idx/posting ix "本酒")))
    ;; 未知 term は空マップ（nil ではない）
    (is (= {} (idx/posting ix "焼酎")))
    (is (sorted? (idx/posting ix "焼酎")))))

(deftest doc-text-lookup
  (let [ix (idx/add-document (idx/empty-index) "日本酒")]
    (is (= "日本酒" (idx/doc-text ix 0)))
    (is (nil? (idx/doc-text ix 99)))))

(deftest stats-basic
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒" "日本語"])
        s  (idx/stats ix)]
    ;; "日本酒"->["日本","本酒"], "日本語"->["日本","本語"]
    ;; term辞書: 日本, 本酒, 本語 = 3。ポスティング総数: 日本(2)+本酒(1)+本語(1)=4
    (is (= 2 (:docs s)))
    (is (= 3 (:terms s)))
    (is (= 4 (:postings s)))
    (is (= (double (/ 4 3)) (:avg-posting s)))))

(deftest segments-and-word-dictionary
  (is (= ["a" "b" "日本酒" "x1"] (tok/segments "A&B 日本酒 x1")))
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒 純米" "日本酒造 蔵元"])]
    (is (= (sorted-map 0 1) (idx/word-posting ix "日本酒")))
    (is (= (sorted-map 1 1) (idx/word-posting ix "日本酒造")))
    (is (= (sorted-map 0 1 1 1) (idx/posting ix "日本")))))

(deftest words-with-prefix-enumerates-sorted
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒 純米" "日本酒造 蔵元" "日本語 文法" "焼酎 芋"])]
    (is (= ["日本酒" "日本酒造"] (map first (idx/words-with-prefix ix "日本酒"))))
    (is (= ["日本語" "日本酒" "日本酒造"] (sort (map first (idx/words-with-prefix ix "日本")))))
    (is (= [] (map first (idx/words-with-prefix ix "存在しない"))))))

(deftest doc-length-and-avgdl
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒" "日本酒 日本酒 日本酒 の 醸造" "焼酎 芋"])]
    (is (= 2 (idx/doc-length ix 0)))
    (is (= 8 (idx/doc-length ix 1)))
    (is (= 4.0 (idx/avg-doc-length ix)))))
