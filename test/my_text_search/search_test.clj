(ns my-text-search.search-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]
            [my-text-search.search :as search]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "mts" "")] (.delete d) (.mkdir d) (str d)))

(deftest search-test
  (let [dir (temp-dir)
        ix  (reduce idx/add-text (idx/empty-index)
                    ["日本酒 純米" "日木酒 タイポ" "日本酒 冷酒" "焼酎 芋" "日本語 文法"])]
    (let [db (lsm/open dir)] (store/persist-index! db ix) (lsm/close db))
    (let [db (lsm/open dir)
          sources {:posting-fn      #(store/get-posting db %)
                   :word-scan-fn    #(store/scan-words db %)
                   :words           (store/all-words db)
                   :word-posting-fn #(store/get-word db %)}]
      (is (= [0 2]     (vec (search/search sources "日本酒"))))
      (is (= [0 2 4]   (vec (search/search sources "日本*"))))
      (is (= [0 1 2 4] (vec (search/search sources "日本酒~"))))
      (lsm/close db))))

(deftest ranked-search-combines-matching-and-boost
  (let [dir (temp-dir)
        ix  (reduce idx/add-document (idx/empty-index)
                    [{:name "獺祭 純米大吟醸"   :description "華やかな日本酒"}
                     {:name "久保田 千寿"       :description "獺祭に似た日本酒"}
                     {:name "獺祭スパークリング" :description "発泡性の酒"}])]
    (let [db (lsm/open dir)] (store/persist-index! db ix) (lsm/close db))
    (let [db (lsm/open dir)]
      ;; ワイルドカード×銘柄重視: 銘柄が獺祭の doc0 が先頭
      (is (= 0 (:doc-id (first (search/ranked-search db {:name 3.0 :description 1.0} "獺祭*")))))
      ;; ワイルドカード×説明重視: 説明に獺祭の doc1 が先頭
      (is (= 1 (:doc-id (first (search/ranked-search db {:name 1.0 :description 3.0} "獺祭*")))))
      (lsm/close db))))

(deftest ranked-search-with-snippets-end-to-end
  (let [dir (temp-dir)
        ix  (reduce idx/add-document (idx/empty-index)
                    [{:name "獺祭" :description "華やかな日本酒です"}
                     {:name "久保田" :description "すっきりした日本酒だ"}])]
    (let [db (lsm/open dir)] (store/persist-index! db ix) (lsm/close db))
    (let [db (lsm/open dir)
          out (search/ranked-search-with-snippets db {:name 1.0 :description 1.0}
                                                  "日本酒" :description)]
      (is (= 2 (count out)))
      (is (every? :snippet out))
      (is (every? #(str/includes? (:snippet %) "日本酒") out))
      (lsm/close db))))
