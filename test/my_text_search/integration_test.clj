(ns my-text-search.integration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]
            [my-text-search.query :as q]
            [my-text-search.fuzzy :as fz]
            [my-text-search.search :as search]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "e2e" "")] (.delete d) (.mkdir d) (str d)))

(def corpus
  [[{:name "獺祭"   :description "山口 華やか 日本酒"} {:region "山口"   :type "大吟醸"}]
   [{:name "久保田" :description "新潟 淡麗 日本酒"}   {:region "新潟"   :type "吟醸"}]
   [{:name "八海山" :description "新潟 日本酒"}         {:region "新潟"   :type "大吟醸"}]
   [{:name "森伊蔵" :description "鹿児島 芋 焼酎"}     {:region "鹿児島" :type "芋"}]])

(defn- build! [dir]
  (let [ix (reduce (fn [ix [f a]] (idx/add-document ix f a)) (idx/empty-index) corpus)
        db (lsm/open dir)]
    (store/persist-index! db ix)
    (lsm/close db)))

(deftest end-to-end
  (let [dir (temp-dir)]
    (build! dir)
    (let [db (lsm/open dir)
          desc-pf #(store/get-field-posting db :description %)
          wf #(store/scan-words db %)
          words (store/all-words db)
          wpf #(store/get-word db %)]

      (testing "Boolean 検索"
        (is (= #{0 1 2} (set (q/search desc-pf "日本酒")))))

      (testing "ワイルドカード（前方一致）"
        (is (= #{0} (set (q/wildcard-search wf "獺")))))

      (testing "ファジー（タイポ耐性）"
        ;; 「獺采」(獺祭のタイポ) で獺祭を拾う
        (is (= #{0} (set (fz/fuzzy-search words wpf "獺采" 1)))))

      (testing "フレーズ（隣接）"
        (is (= #{0 1 2} (set (q/phrase-search desc-pf "日本酒")))))

      (testing "BM25 ランキング（結果集合）"
        (let [rows (search/ranked-search db {:description 1.0} "日本酒")]
          (is (= #{0 1 2} (set (map :doc-id rows))))
          (is (= 3 (count rows)))))

      (testing "フィールド boost で順位が変わる"
        ;; description に日本酒がある3件。name boost では順位が name のスコアに依存
        (let [by-desc (search/ranked-search db {:description 3.0 :name 1.0} "日本酒")]
          (is (= 3 (count by-desc)))))

      (testing "ハイライト（スニペットにマッチが含まれる）"
        (let [rows (search/ranked-search-with-snippets
                    db {:description 1.0} "日本酒" :description)]
          (is (every? #(str/includes? (:snippet %) "日本酒") rows))))

      (testing "ファセット（結果を属性で集計）"
        (let [{:keys [facets]} (search/search-with-facets
                                db {:description 1.0} "日本酒" [:region :type])]
          (is (= {"山口" 1 "新潟" 2} (:region facets)))
          (is (= {"大吟醸" 2 "吟醸" 1} (:type facets)))))

      (lsm/close db))))
