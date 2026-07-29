(ns my-text-search.index-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.index :as idx]
            [my-text-search.tokenizer :as tok]))

(deftest empty-index-shape
  (let [ix (idx/empty-index)]
    (is (= {} (:docs ix)))
    (is (zero? (:next-id ix)))
    (is (empty? (:fields ix)))
    (is (sorted? (:fields ix)))
    (is (empty? (:words ix)))
    (is (sorted? (:words ix)))))

(deftest postings-stay-sorted
  ;; 投入順に関わらず term 辞書はソート順を保つ
  (let [ix (idx/add-text (idx/empty-index) "日本酒")
        p  (get-in ix [:fields idx/default-field :postings])]
    (is (sorted? p))
    (is (= ["日本" "本酒"] (keys p)))))

(deftest add-one-document
  (let [ix (idx/add-text (idx/empty-index) "日本酒")]
    (is (= 1 (:next-id ix)))
    (is (= "日本酒" (idx/doc-text ix 0)))
    (is (= {0 [0]} (idx/posting ix "日本")))
    (is (= #{0} (set (keys (idx/posting ix "日本")))))
    (is (= #{0} (set (keys (idx/posting ix "本酒")))))))

(deftest shared-term-collects-doc-ids-sorted
  (let [ix (-> (idx/empty-index)
               (idx/add-text "日本酒")     ; id 0
               (idx/add-text "日本語")     ; id 1
               (idx/add-text "本日"))]     ; id 2
    (is (= {0 [0] 1 [0]} (idx/posting ix "日本")))   ; 「日本」は0,1 に各1回
    (is (= [0 1] (keys (idx/posting ix "日本"))))))

(deftest duplicate-term-in-one-doc-counts-tf
  ;; 「あああ」は bigram 「ああ」を2回含むので TF=2
  (let [ix (idx/add-text (idx/empty-index) "あああ")]
    (is (= {0 [0 1]} (idx/posting ix "ああ")))))

(deftest reduce-over-corpus
  (let [ix (reduce idx/add-text (idx/empty-index)
                   ["日本酒" "純米酒" "焼酎"])]
    (is (= 3 (:next-id ix)))
    (is (= 3 (count (:docs ix))))))

(deftest posting-lookup
  (let [ix (-> (idx/empty-index)
               (idx/add-text "日本酒")    ; id 0
               (idx/add-text "日本語"))]   ; id 1
    (is (= {0 [0] 1 [0]} (idx/posting ix "日本")))
    (is (= {0 [1]} (idx/posting ix "本酒")))
    ;; 未知 term は空マップ（nil ではない）
    (is (= {} (idx/posting ix "焼酎")))
    (is (sorted? (idx/posting ix "焼酎")))))

(deftest doc-text-lookup
  (let [ix (idx/add-text (idx/empty-index) "日本酒")]
    (is (= "日本酒" (idx/doc-text ix 0)))
    (is (nil? (idx/doc-text ix 99)))))

(deftest stats-basic
  (let [ix (reduce idx/add-text (idx/empty-index)
                   ["日本酒" "日本語"])
        s  (idx/stats ix)]
    ;; "日本酒"->["日本","本酒"], "日本語"->["日本","本語"]
    ;; term辞書: 日本, 本酒, 本語 = 3（:text フィールド）
    ;; word辞書: 日本酒, 日本語 = 2
    (is (= 2 (:docs s)))
    (is (= {idx/default-field 3} (:fields s)))
    (is (= 2 (:words s)))))

(deftest segments-and-word-dictionary
  (is (= ["a" "b" "日本酒" "x1"] (tok/segments "A&B 日本酒 x1")))
  (let [ix (reduce idx/add-text (idx/empty-index)
                   ["日本酒 純米" "日本酒造 蔵元"])]
    (is (= {0 [0]} (idx/word-posting ix "日本酒")))
    (is (= {1 [0]} (idx/word-posting ix "日本酒造")))
    (is (= {0 [0] 1 [0]} (idx/posting ix "日本")))))

(deftest words-with-prefix-enumerates-sorted
  (let [ix (reduce idx/add-text (idx/empty-index)
                   ["日本酒 純米" "日本酒造 蔵元" "日本語 文法" "焼酎 芋"])]
    (is (= ["日本酒" "日本酒造"] (map first (idx/words-with-prefix ix "日本酒"))))
    (is (= ["日本語" "日本酒" "日本酒造"] (sort (map first (idx/words-with-prefix ix "日本")))))
    (is (= [] (map first (idx/words-with-prefix ix "存在しない"))))))

(deftest doc-length-and-avgdl
  (let [ix (reduce idx/add-text (idx/empty-index)
                   ["日本酒" "日本酒 日本酒 日本酒 の 醸造" "焼酎 芋"])]
    (is (= 2 (idx/doc-length ix 0)))
    (is (= 8 (idx/doc-length ix 1)))
    (is (= 4.0 (idx/avg-doc-length ix)))))

(deftest posting-records-positions
  (let [ix (idx/add-text (idx/empty-index) "日本酒 日本酒 の 醸造")]
    ;; 「日本」は位置0と2（日本酒が2回）
    (is (= {0 [0 2]} (idx/posting ix "日本")))
    (is (= {0 [5]}   (idx/posting ix "醸造")))
    ;; tf は位置数
    (is (= 2 (count (get (idx/posting ix "日本") 0))))))
