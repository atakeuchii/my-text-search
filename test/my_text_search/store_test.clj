(ns my-text-search.store-test
  (:require [clojure.test :refer [deftest is]]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx :refer [default-field]]
            [my-text-search.query :as q]
            [my-text-search.store :as store]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "mts" "")]
    (.delete d) (.mkdir d) (str d)))

(deftest posting-persists-across-restart
  (let [dir (temp-dir)]
    (let [db (lsm/open dir)]
      (store/put-field-posting! db default-field "日本" (sorted-map 0 [0] 1 [0 1 2] 5 [2 3]))
      (is (= {0 [0] 1 [0 1 2] 5 [2 3]} (store/get-posting db "日本")))
      (is (nil? (store/get-posting db "焼酎")))     ; 未知の語
      (lsm/close db))
    (let [db2 (lsm/open dir)]                        ; 再起動
      (is (= {0 [0] 1 [0 1 2] 5 [2 3]} (store/get-posting db2 "日本")))
      (lsm/close db2))))

(deftest index-persists-and-searches-across-restart
  (let [dir (temp-dir)]
    (let [ix (reduce idx/add-text (idx/empty-index)
                     ["日本酒の醸造" "純米大吟醸の精米" "日本語の文法"])
          db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= {0 [0] 2 [0]} (store/get-posting db "日本")))
      (is (= {0 [1]} (store/get-posting db "本酒")))
      (is (= "日本酒の醸造" (store/get-doc db 0)))
      (is (= 3 (store/get-next-id db)))
      (lsm/close db))))

(deftest q-search-over-store
  (let [dir (temp-dir)]
    (let [ix (reduce idx/add-text (idx/empty-index)
                     ["日本酒の醸造" "純米大吟醸の精米" "日本語の文法"])
          db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)
          pf #(store/get-posting db %)]
      (is (= [0] (vec (q/search pf "日本酒"))))
      (is (= [0 2] (vec (q/search pf "日本酒" :op :or))))
      (is (= [2] (vec (q/search pf "日本語"))))
      (is (= [] (vec (q/search pf "焼酎"))))
      (lsm/close db))))

(deftest search-docs-attaches-text
  (let [dir (temp-dir)]
    (let [ix (reduce idx/add-text (idx/empty-index)
                     ["日本酒の醸造" "日本語の文法"])
          db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)
          pf #(store/get-posting db %)
          df #(store/get-doc db %)]
      (is (= [{:doc-id 0 :text "日本酒の醸造"}]
             (q/search-docs pf df "日本酒")))
      (is (= [{:doc-id 0 :text "日本酒の醸造"}
              {:doc-id 1 :text "日本語の文法"}]
             (q/search-docs pf df "日本" :op :or)))
      (is (= [{:doc-id 0 :text "日本酒の醸造"}]
             (q/search-docs pf df "日本" :op :or :limit 1)))
      (lsm/close db))))

(deftest word-dictionary-persists
  (let [dir (temp-dir)]
    (let [ix (reduce idx/add-text (idx/empty-index) ["日本酒 純米" "日本酒造 蔵元"])
          db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= {0 [0]} (store/get-word db "日本酒")))
      (is (= {1 [0]} (store/get-word db "日本酒造")))
      (lsm/close db))))

(deftest scan-words-matches-in-memory
  (let [dir (temp-dir)
        ix  (reduce idx/add-text (idx/empty-index)
                    ["日本酒 純米" "日本酒造 蔵元" "日本語 文法" "焼酎 芋"])]
    (let [db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= (vec (idx/words-with-prefix ix "日本酒"))
             (vec (store/scan-words db "日本酒"))))
      (is (= [] (vec (store/scan-words db "存在しない"))))
      (lsm/close db))))

(deftest wildcard-over-store
  (let [dir (temp-dir)
        ix  (reduce idx/add-text (idx/empty-index)
                    ["日本酒 純米" "日本酒造 蔵元" "日本語 文法" "STORE front"])]
    (let [db (lsm/open dir)] (store/persist-index! db ix) (lsm/close db))
    (let [db (lsm/open dir)
          wf #(store/scan-words db %)
          pf #(store/get-posting db %)]
      (is (= [0 1] (vec (q/wildcard-search wf "日本酒"))))
      (is (= [3]   (vec (q/wildcard-search wf "STORE"))))
      (is (= [0 1] (vec (q/search-any pf wf "日本酒*"))))
      (lsm/close db))))

(deftest all-words-returns-full-dictionary
  (let [dir (temp-dir)
        ix  (reduce idx/add-text (idx/empty-index)
                    ["日本酒 純米" "日本酒造 蔵元" "焼酎 芋"])]
    (let [db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= #{"日本酒" "純米" "日本酒造" "蔵元" "焼酎" "芋"}
             (set (store/all-words db))))
      (lsm/close db))))

(deftest doc-length-persists
  (let [dir (temp-dir)
        ix  (reduce idx/add-text (idx/empty-index)
                    ["日本酒" "日本酒 日本酒 日本酒 の 醸造" "焼酎 芋"])]
    (let [db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= 8 (store/get-doc-length db 1)))
      (is (= 12 (store/get-total-len db)))
      (is (= 4.0 (store/avg-doc-length db)))
      (lsm/close db))))

(deftest doc-values-persist
  (let [dir (temp-dir)
        ix  (-> (idx/empty-index)
                (idx/add-document {:name "獺祭"} {:region "山口"})
                (idx/add-document {:name "久保田"} {:region "新潟"}))]
    (let [db (lsm/open dir)] (store/persist-index! db ix) (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= "山口" (store/get-doc-value db 0 :region)))
      (is (= "新潟" (store/get-doc-value db 1 :region)))
      (lsm/close db))))
