(ns my-text-search.index
  (:require [my-text-search.tokenizer :as tok]))

(defn empty-index
  []
  {:postings (sorted-map)
   :words (sorted-map)
   :docs {}
   :doc-lengths {}
   :next-id 0})

(defn- add-freqs
  [m freqs doc-id]
  (reduce (fn [acc [k cnt]]
            (update acc k (fnil assoc (sorted-map)) doc-id cnt))
          m freqs))

(defn add-document
  [index text]
  (let [doc-id (:next-id index)
        term-freqs (frequencies (tok/tokenize text))
        word-freqs (frequencies (tok/segments text))
        dl (reduce + 0 (vals term-freqs))]
    (-> index
        (update :docs assoc doc-id text)
        (update :doc-lengths assoc doc-id dl)
        (update :postings add-freqs term-freqs doc-id)
        (update :words add-freqs word-freqs doc-id)
        (assoc :next-id (inc doc-id)))))

(defn posting
  [index term]
  (get-in index [:postings term] (sorted-map)))

(defn word-posting
  [index word]
  (get-in index [:words word] (sorted-map)))

(defn doc-text
  [index doc-id]
  (get-in index [:docs doc-id]))

(defn doc-length
  [index doc-id]
  (get-in index [:doc-lengths doc-id] 0))

(defn avg-doc-length
  [index]
  (let [ls (vals (:doc-lengths index))]
    (if (empty? ls)
      0.0
      (/ (double (reduce + 0 ls)) (count ls)))))

(defn words-with-prefix
  [index prefix]
  (->> (subseq (:words index) >= prefix)
       (take-while (fn [[w _]] (.startsWith ^String w prefix)))
       (map (fn [[w ids]] [w ids]))))

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
