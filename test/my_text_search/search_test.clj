(ns my-text-search.search-test
  (:require [clojure.test :refer [deftest is]]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]
            [my-text-search.search :as search]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "mts" "")] (.delete d) (.mkdir d) (str d)))

(deftest search-test
  (let [dir (temp-dir)
        ix  (reduce idx/add-document (idx/empty-index)
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
