(ns my-text-search.search
  (:require [clojure.string :as str]
            [my-text-search.tokenizer :as tok]
            [my-text-search.query :as q]
            [my-text-search.facet :as facet]
            [my-text-search.fuzzy :as fz]
            [my-text-search.score :as score]
            [my-text-search.store :as store]
            [my-text-search.highlight :as hl]))

(defn search
  [sources query & {:keys [op k] :or {op :and k 1}}]
  (let [{:keys [posting-fn word-scan-fn words word-posting-fn]} sources]
    (cond
      (str/ends-with? query "*")
      (q/wildcard-search word-scan-fn (subs query 0 (dec (count query))))
      
      (str/ends-with? query "~")
      (fz/fuzzy-search words word-posting-fn
                       (tok/normalize (subs query 0 (dec (count query)))) k)
      
      :else
      (q/search posting-fn query :op op))))

(defn- field-sources
  "field-boosts {フィールド -> boost} から score 用の field-sources を作る。"
  [store field-boosts]
  (for [[field boost] field-boosts]
    {:field field
     :posting-fn #(store/get-field-posting store field %)
     :n-docs (store/doc-count store)
     :dl-fn #(store/get-field-length store field %)
     :avgdl (store/field-avg-doc-length store field)
     :boost boost}))

(defn- strip-notation
  [^String q]
  (if (or (str/ends-with? q "*") (str/ends-with? q "~"))
    (subs q 0 (dec (count q))) q))

(defn ranked-search
  "マッチング(記法で振り分け)で候補を集め、field-boosts で合成 BM25 ランキング。
   field-boosts: {:name 3.0 :description 1.0}"
  [store field-boosts query & {:keys [op k] :or {op :and k 1}}]
  (let [srcs (field-sources store field-boosts)
        cands (cond
                (str/ends-with? query "*")
                (q/wildcard-search #(store/scan-words store %)
                                   (strip-notation query))

                (str/ends-with? query "~")
                (fz/fuzzy-search (store/all-words store)
                                 #(store/get-word store %)
                                 (tok/normalize (strip-notation query))
                                 k)

                :else
                (q/union-search (map :posting-fn srcs) query :op op))]
    (score/bm25-multi-field-rank srcs (strip-notation query) cands)))

(defn ranked-search-with-snippets
  "マッチング→フィールドboostランキング→スニペット付与、を一気通貫で行う。
   snippet-field: スニペットを作る対象フィールド。"
  [store field-boosts query snippet-field & {:keys [op k window] :or {op :and k 1 window 30}}]
  (let [rows (ranked-search store field-boosts query :op op :k k)]
    (hl/with-snippets #(store/get-field-doc store snippet-field %)
      (strip-notation query) rows :window window)))

(defn search-with-facets
  "検索→フィールドboostランキング + 指定属性のファセット。
   facet-attrs: 集計する属性のリスト [:region :type]。
   {:results [{:doc-id :score} ...] :facets {属性 -> {値 -> 件数}}} を返す。"
  [store field-boosts query facet-attrs & {:keys [op k] :or {op :and k 1}}]
  (let [rows (ranked-search store field-boosts query :op op :k k)
        ids (map :doc-id rows)
        vfns (into {} (for [a facet-attrs] [a #(store/get-doc-value store % a)]))]
    {:results rows
     :facets (facet/facets vfns ids)}))
