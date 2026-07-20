(ns my-text-search.index
  (:require [my-text-search.tokenizer :as tok]))

(defn empty-index
  []
  {:postings (sorted-map)
   :docs {}
   :next-id 0})

(defn add-document
  [index text]
  (let [doc-id (:next-id index)
        terms (distinct (tok/tokenize text))]
    (-> index
        (update :docs assoc doc-id text)
        (update :postings
                (fn [postings]
                  (reduce (fn [p t]
                            (update p t (fnil conj (sorted-set)) doc-id))
                          postings
                          terms)))
        (assoc :next-id (inc doc-id)))))

(defn posting
  [index term]
  (get-in index [:postings term] (sorted-set)))

(defn doc-text
  [index doc-id]
  (get-in index [:docs doc-id]))

(defn stats
  "索引の健全性を見るための統計。
   :docs         文書数
   :terms        term辞書サイズ（ユニーク term 数）= インデックス容量の目安
   :postings     全ポスティング長の合計（term×文書 の総ペア数）
   :avg-posting  1 term あたり平均文書数 = 検索コストの目安"
  [index]
  (let [postings (:postings index)
        term-count (count postings)
        total (reduce + 0 (map count (vals postings)))]
    {:docs (count (:docs index))
     :terms term-count
     :postings total
     :avg-posting (if (zero? term-count)
                    0.0
                    (double (/ total term-count)))}))
