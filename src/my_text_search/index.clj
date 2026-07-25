(ns my-text-search.index
  (:require [my-text-search.tokenizer :as tok]))

(defn empty-index
  []
  {:postings (sorted-map)
   :words (sorted-map)
   :docs {}
   :next-id 0})

(defn add-document
  [index text]
  (let [doc-id (:next-id index)
        terms (distinct (tok/tokenize text))
        words (distinct (tok/segments text))]
    (letfn [(add-all [m keys doc-id]
              (reduce
               (fn [acc k] (update acc k (fnil conj (sorted-set)) doc-id))
               m keys))]
      (-> index
          (update :docs assoc doc-id text)
          (update :postings add-all terms doc-id)
          (update :words add-all words doc-id)
          (assoc :next-id (inc doc-id))))))

(defn posting
  [index term]
  (get-in index [:postings term] (sorted-set)))

(defn word-posting
  [index word]
  (get-in index [:words word] (sorted-set)))

(defn words-with-prefix
  [index prefix]
  (->> (subseq (:words index) >= prefix)
       (take-while (fn [[w _]] (.startsWith ^String w prefix)))
       (map (fn [[w ids]] [w ids]))))

(defn doc-text
  [index doc-id]
  (get-in index [:docs doc-id]))

(defn stats
  "索引の健全性を見るための統計。
   :docs         文書数
   :terms        term辞書サイズ（ユニーク term 数）= インデックス容量の目安
   :words        語辞書サイズ
   :postings     全ポスティング長の合計（term×文書 の総ペア数）
   :avg-posting  1 term あたり平均文書数 = 検索コストの目安"
  [index]
  (let [postings (:postings index)
        term-count (count postings)
        total (reduce + 0 (map count (vals postings)))]
    {:docs (count (:docs index))
     :terms term-count
     :words (count (:words index))
     :postings total
     :avg-posting (if (zero? term-count)
                    0.0
                    (double (/ total term-count)))}))
