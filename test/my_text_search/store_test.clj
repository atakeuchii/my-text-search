(ns my-text-search.store-test
  (:require [clojure.test :refer [deftest is]]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "mts" "")]
    (.delete d) (.mkdir d) (str d)))

(deftest posting-persists-across-restart
  (let [dir (temp-dir)]
    (let [db (lsm/open dir)]
      (store/put-posting! db "日本" (sorted-set 0 1 5))
      (is (= (sorted-set 0 1 5) (store/get-posting db "日本")))
      (is (nil? (store/get-posting db "焼酎")))     ; 未知の語
      (lsm/close db))
    (let [db2 (lsm/open dir)]                        ; 再起動
      (is (= (sorted-set 0 1 5) (store/get-posting db2 "日本")))
      (lsm/close db2))))

(deftest index-persists-and-searches-across-restart
  (let [dir (temp-dir)]
    (let [ix (reduce idx/add-document (idx/empty-index)
                     ["日本酒の醸造" "純米大吟醸の精米" "日本語の文法"])
          db (lsm/open dir)]
      (store/persist-index! db ix)
      (lsm/close db))
    (let [db (lsm/open dir)]
      (is (= (sorted-set 0 2) (store/get-posting db "日本")))
      (is (= (sorted-set 0)   (store/get-posting db "本酒")))
      (is (= "日本酒の醸造"    (store/get-doc db 0)))
      (is (= 3 (store/get-next-id db)))
      (lsm/close db))))
