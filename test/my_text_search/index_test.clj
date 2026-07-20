(ns my-text-search.index-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.index :as idx]))

(deftest empty-index-shape
  (let [ix (idx/empty-index)]
    (is (= {} (:docs ix)))
    (is (zero? (:next-id ix)))
    (is (empty? (:postings ix)))
    (is (sorted? (:postings ix)))))

(deftest empty-index-postings-stay-sorted
  (let [p (-> (:postings (idx/empty-index))
              (assoc "本酒" #{0})
              (assoc "日本" #{0}))]
    (is (= ["日本" "本酒"] (keys p)))))

(deftest add-one-document
  (let [ix (idx/add-document (idx/empty-index) "日本酒")]
    (is (= 1 (:next-id ix)))
    (is (= "日本酒" (get-in ix [:docs 0])))
    (is (= #{0} (get-in ix [:postings "日本"])))
    (is (= #{0} (get-in ix [:postings "本酒"])))))

(deftest shared-term-collects-doc-ids-sorted
  (let [ix (-> (idx/empty-index)
               (idx/add-document "日本酒")     ; id 0
               (idx/add-document "日本語")     ; id 1
               (idx/add-document "本日"))]     ; id 2
    (is (= #{0 1} (get-in ix [:postings "日本"])))   ; 「日本」は0,1
    (is (= [0 1] (seq (get-in ix [:postings "日本"]))))))

(deftest duplicate-term-in-one-doc-counted-once
  (let [ix (idx/add-document (idx/empty-index) "あああ")]
    (is (= #{0} (get-in ix [:postings "ああ"])))))

(deftest reduce-over-corpus
  (let [ix (reduce idx/add-document (idx/empty-index)
                   ["日本酒" "純米酒" "焼酎"])]
    (is (= 3 (:next-id ix)))
    (is (= 3 (count (:docs ix))))))

(deftest posting-lookup
  (let [ix (-> (idx/empty-index)
               (idx/add-document "日本酒")    ; id 0
               (idx/add-document "日本語"))]   ; id 1
    (is (= #{0 1} (idx/posting ix "日本")))
    (is (= #{0}   (idx/posting ix "本酒")))
    ;; 未知 term は空集合（nil ではない）
    (is (= #{} (idx/posting ix "焼酎")))
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
