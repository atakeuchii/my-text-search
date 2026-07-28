(ns my-text-search.score
  (:require [my-text-search.tokenizer :as tok]))

(def default-k1 1.2)
(def default-b 0.75)

(defn idf
  "log(N / DF)。DF=0 は 0 とする。希少な語ほど大きい。"
  ^double [^long n-docs ^long df]
  (if (zero? df)
    0.0
    (Math/log (/ (double n-docs) (double df)))))

(defn bm25-idf
  "BM25 の IDF: log(1 + (N - df + 0.5)/(df + 0.5))。負にならず DF≈N でも安定。"
  ^double [^long n-docs ^long df]
  (Math/log (+ 1.0 (/ (+ (- n-docs df) 0.5) (+ df 0.5)))))

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

(defn bm25-term-score
  "1 term の BM25 寄与。tf 飽和(k1) と文書長正規化(b)。"
  [idf tf dl avgdl k1 b]
  (if (or (zero? tf) (<= avgdl 0.0))
    0.0
    (let [norm (+ (- 1.0 b) (* b (/ (double dl) avgdl)))
          denom (+ tf (* k1 norm))]
      (* idf (/ (* tf (+ k1 1.0)) denom)))))

(defn bm25-score
  "文書 doc-id の BM25 スコア。
   posting-fn: term -> {文書ID -> TF}, dl-fn: 文書ID -> 文書長。"
  [posting-fn n-docs dl-fn avgdl terms doc-id
   & {:keys [k1 b] :or {k1 default-k1 b default-b}}]
  (let [dl (dl-fn doc-id)]
    (reduce (fn [acc term]
              (let [post (posting-fn term)
                    df (count post)
                    tf (get post doc-id 0)]
                (+ acc (bm25-term-score (bm25-idf n-docs df) tf dl avgdl k1 b))))
            0.0 terms)))

(defn rank
  "候補文書 doc-ids を TF-IDF 降順に並べ、[{:doc-id :score} ...] を返す。
   同点は doc-id 昇順で安定化する。"
  [posting-fn n-docs query doc-ids]
  (let [terms (distinct (tok/tokenize query))]
    (->> doc-ids
         (map (fn [id] {:doc-id id
                        :score (tf-idf-score posting-fn n-docs terms id)}))
         (sort-by (juxt (comp - :score) :doc-id)))))

(defn bm25-rank
  "候補文書を BM25 降順に並べる。[{:doc-id :score} ...]。同点は doc-id 昇順。"
  [posting-fn n-docs dl-fn avgdl query doc-ids
   & {:keys [k1 b] :or {k1 default-k1 b default-b}}] 
  (let [terms (distinct (tok/tokenize query))]
    (->> doc-ids
         (map (fn [id] {:doc-id id
                        :score (bm25-score posting-fn n-docs dl-fn avgdl terms id :k1 k1 :b b)}))
         (sort-by (juxt (comp - :score) :doc-id)))))

(defn bm25-multi-field-score
  "複数フィールドの BM25 を boost で合成した文書スコア。
   field-sources: [{:field :posting-fn :n-docs :dl-fn :avgdl :boost} ...]
   posting-fn: term -> {doc->tf}（そのフィールドの）, dl-fn: doc-id -> そのフィールドの長さ。"
  [field-sources query doc-id & {:keys [k1 b] :or {k1 default-k1 b default-b}}]
  (let [terms (distinct (tok/tokenize query))]
    (reduce (fn [acc {:keys [posting-fn n-docs dl-fn avgdl boost]}]
              (+ acc (* boost (bm25-score posting-fn n-docs dl-fn avgdl terms doc-id :k1 k1 :b b))))
            0.0 field-sources)))

(defn bm25-multi-field-rank
  "候補文書を、フィールド合成スコアの降順に並べる。"
  [field-sources query doc-ids & {:keys [k1 b] :or {k1 default-k1 b default-b}}]
  (->> doc-ids
       (map (fn [id] {:doc-id id
                      :score (bm25-multi-field-score field-sources query id :k1 k1 :b b)}))
       (sort-by (juxt (comp - :score) :doc-id))))
