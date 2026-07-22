(ns my-text-search.query
  (:require [my-text-search.tokenizer :as tok]))

(defn- intersect2
  "2つの昇順 seq の積集合を昇順の lazy-seq で返す。"
  [xs ys]
  (lazy-seq
   (when (and (seq xs) (seq ys))
     (let [x (first xs)
           y (first ys)]
       (cond
         (< x y) (intersect2 (rest xs) ys)
         (> x y) (intersect2 xs (rest ys))
         :else (cons x (intersect2 (rest xs) (rest ys))))))))

(defn- union2
  "2つの昇順 seq の和集合を昇順の lazy-seq で返す。"
  [xs ys]
  (lazy-seq
   (cond
     (empty? xs) ys
     (empty? ys) xs
     :else (let [x (first xs)
                 y (first ys)]
             (cond
               (< x y) (cons x (union2 (rest xs) ys))
               (> x y) (cons y (union2 xs (rest ys)))
               :else (cons x (union2 (rest xs) (rest ys))))))))

(defn and-postings
  [postings]
  (if (empty? postings)
    ()
    (reduce intersect2 postings)))

(defn or-postings
  [postings]
  (if (empty? postings)
    ()
    (reduce union2 postings)))

(defn search
  "posting-fn: term -> 昇順文書ID列(nil可)。
   query を index と同じトークナイザで分解し、AND(既定)/OR で結合してヒット文書IDの昇順列を返す。"
  [posting-fn query & {:keys [op] :or {op :and}}]
  (let [terms (distinct (tok/tokenize query))]
    (if (empty? terms)
      ()
      (let [postings (map #(or (posting-fn %) ()) terms)]
        (case op
          :and (and-postings postings)
          :or (or-postings postings))))))

(defn hydrate
  "文書ID列に本文を添える。doc-fn: 文書ID -> 本文。
   [{:doc-id id :text \"...\"} ...] を返す。"
  [doc-fn doc-ids]
  (map (fn [id] {:doc-id id :text (doc-fn id)}) doc-ids))

(defn search-docs
  "検索して本文付き結果を返す便宜関数。
   posting-fn: term->ポスティング, doc-fn: 文書ID->本文。
   :op :and|:or  :limit 件数(先頭k件。現状はID順)。"
  [posting-fn doc-fn query & {:keys [op limit] :or {op :and}}]
  (let [ids (search posting-fn query :op op)
        ids (if limit (take limit ids) ids)]
    (hydrate doc-fn ids)))
