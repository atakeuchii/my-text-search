(ns my-text-search.score
  (:require [my-text-search.tokenizer :as tok]))

(defn idf
  "log(N / DF)。DF=0 は 0 とする。希少な語ほど大きい。"
  ^double [^long n-docs ^long df]
  (if (zero? df)
    0.0
    (Math/log (/ (double n-docs) (double df)))))

(defn tf-idf-score
  "文書 doc-id の、クエリ term 群に対する TF-IDF スコア。
   posting-fn: term -> {文書ID -> TF}。"
  ^double [posting-fn n-docs terms doc-id]
  (reduce
   (fn [acc term]
     (let [post (posting-fn term)
           df (count post)
           tf (get post doc-id 0)]
       (if (pos? tf)
         (+ acc (* tf (idf n-docs df)))
         acc)))
   0.0
   terms))

(defn rank
  "候補文書 doc-ids を TF-IDF 降順に並べ、[{:doc-id :score} ...] を返す。
   同点は doc-id 昇順で安定化する。"
  [posting-fn n-docs query doc-ids]
  (let [terms (distinct (tok/tokenize query))]
    (->> doc-ids
         (map (fn [id] {:doc-id id
                        :score (tf-idf-score posting-fn n-docs terms id)}))
         (sort-by (juxt (comp - :score) :doc-id)))))
