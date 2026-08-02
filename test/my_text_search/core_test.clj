(ns my-text-search.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [my-text-search.core :as ts]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "core" "")] (.delete d) (.mkdir d) (str d)))

(def docs
  [{:fields {:name "獺祭"   :description "山口 華やか 日本酒"} :attrs {:region "山口" :type "大吟醸"}}
   {:fields {:name "久保田" :description "新潟 淡麗 日本酒"}   :attrs {:region "新潟" :type "吟醸"}}
   {:fields {:name "八海山" :description "新潟 日本酒"}         :attrs {:region "新潟" :type "大吟醸"}}])

(deftest build-open-search-close
  (let [dir (temp-dir)]
    (ts/build! dir docs {:wal-fsync 1000})
    (let [db (ts/open dir)
          {:keys [results facets]}
          (ts/search db "日本酒"
                     :fields {:description 1.0} :facet-attrs [:region] :snippet-field :description)]
      (is (= #{0 1 2} (set (map :doc-id results))))
      (is (every? #(str/includes? (:snippet %) "日本酒") results))
      (is (= {"山口" 1 "新潟" 2} (:region facets)))
      (ts/close db))))

(deftest add-appends-and-persists
  (let [dir (temp-dir)]
    (ts/build! dir docs {:wal-fsync 1000})
    (testing "追記: next-id 継続、追記 doc-id を返す"
      (let [db (ts/open dir)
            new-id (ts/add! db {:fields {:name "獺祭" :description "山口 スパークリング 日本酒"}
                                :attrs {:region "山口" :type "泡"}})]
        (is (= 3 new-id))
        (ts/close db)))
    (testing "再起動しても追記が残り、既存文書も引ける"
      (let [db (ts/open dir)
            {:keys [results facets]}
            (ts/search db "日本酒" :fields {:description 1.0} :facet-attrs [:region :type])]
        (is (= #{0 1 2 3} (set (map :doc-id results))))         ; 4件
        (is (= {"山口" 2 "新潟" 2} (:region facets)))            ; 山口 1→2
        (is (= {"大吟醸" 2 "吟醸" 1 "泡" 1} (:type facets)))     ; 泡 が追加
        ;; 追記文書だけの語も引ける
        (is (= #{3} (set (map :doc-id (:results (ts/search db "スパークリング"
                                                           :fields {:description 1.0}))))))
        (ts/close db)))))
