(ns my-text-search.core
  (:require [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]
            [my-text-search.search :as search]
            [my-text-search.highlight :as hl]))

(defn build!
  "文書を索引化して dir に永続化する。
   docs: [{:fields {フィールド テキスト} :attrs {属性 値}} ...] (:attrs 省略可)
   opts: my-storage の open オプション（例 {:wal-fsync 1000}）。"
  [dir docs & [opts]]
  (let [ix (reduce (fn [ix {:keys [fields attrs]}]
                     (idx/add-document ix fields (or attrs {})))
                   (idx/empty-index) docs)
        db (lsm/open dir (or opts {}))]
    (store/persist-index! db ix)
    (lsm/close db)
    dir))

(defn add!
  "開いている db に文書を1件追記する。doc-id は store の next-id を継続。
   注意: term ごとに read-modify-write するため、大量追加には不向き。"
  [db {:keys [fields attrs]}]
  (let [doc-id (store/get-next-id db)
        ix (-> (assoc (idx/empty-index) :next-id doc-id)
               (idx/add-document fields (or attrs {})))]
    (doseq [[fname f-idx] (:fields ix)]
      (doseq [[term dp] (:postings f-idx)]
        (store/merge-field-posting! db fname term dp))
      (doseq [[id len] (:doc-lengths f-idx)]
        (store/put-field-length! db fname id len))
      (store/set-field-total! db fname
                              (+ (store/get-field-total db fname)
                                 (reduce + 0 (vals (:doc-lengths f-idx))))))
    (doseq [[word dp] (:words ix)] (store/merge-word! db word dp))
    (doseq [[id fs] (:docs ix)]
      (doseq [[fname text] fs] (store/put-field-doc! db fname id text)))
    (doseq [[id at] (:doc-values ix)]
      (doseq [[a v] at] (store/put-doc-value! db id a v)))
    (store/set-next-id! db (inc doc-id))
    doc-id))

(defn open
  "検索用にインデックスを開く。使い終わったら close する。"
  ([dir] (open dir {}))
  ([dir opts] (lsm/open dir opts)))

(defn close [db] (lsm/close db))

(defn- strip [q]
  (if (and (seq q) (#{\* \~} (last q))) (subs q 0 (dec (count q))) q))

(defn search
  "検索してランキング結果 + ファセット(+スニペット)を返す。
   :fields {フィールド boost}  :facet-attrs [属性...]  :snippet-field フィールド
   :op :and|:or  :k ファジー距離
   => {:results [{:doc-id :score (:snippet)}] :facets {属性 {値 件数}}}"
  [db query & {:keys [fields facet-attrs snippet-field op k]
               :or {fields {:text 1.0} facet-attrs [] op :and k 1}}]
  (let [{:keys [results facets]} (search/search-with-facets db fields query facet-attrs :op op :k k)
        results (if snippet-field
                  (hl/with-snippets #(store/get-field-doc db snippet-field %) (strip query) results)
                  results)]
    {:results (vec results) :facets facets}))
