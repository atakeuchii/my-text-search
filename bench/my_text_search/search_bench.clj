(ns my-text-search.search-bench
  (:require [clojure.string :as str]
            [my-storage.core :as lsm]
            [my-text-search.index :as idx]
            [my-text-search.store :as store]
            [my-text-search.query :as q])
  (:import [java.io File]))
 
(def vocab
  ["日本酒" "純米" "大吟醸" "吟醸" "本醸造" "焼酎" "泡盛" "醸造" "精米"
   "山田錦" "五百万石" "麹" "酵母" "杜氏" "蔵元" "冷酒" "熱燗" "辛口" "甘口"])

(defn gen-corpus [n words-per-doc]
  (vec (for [_ (range n)]
         (str/join " " (repeatedly words-per-doc #(rand-nth vocab))))))

(defn- dir-size ^long [^File d]
  (reduce + 0 (map #(.length ^File %) (filter #(.isFile ^File %) (file-seq d)))))

(defn- median [xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn- bench-search [db label f queries]
  (dotimes [_ 3]
    (doall (map f queries)))
  (let [times (for [query queries]
                (let [t0 (System/nanoTime)]
                  (doall (f query))
                  (/ (- (System/nanoTime) t0) 1e6)))]
    (println (format " %-12s median %.3f ms / query" label (median times)))))

(defn -main [& args]
  (let [n (if (seq args)
            (Long/parseLong (first args))
            200)
        corpus (gen-corpus n 8)
        dir (str (File/createTempFile "bench" "") ".d")]
    (.delete (File. dir))
    (.mkdirs (File. dir))
    (println (format "=== 文書数 %d ===" n))
    (let [t0 (System/nanoTime)
          ix (reduce idx/add-text (idx/empty-index) corpus)
          build-ms (/ (- (System/nanoTime) t0) 1e6)]
      (println (format "索引構築(オンメモリ): %.1f ms" build-ms))
      (let [db (lsm/open dir {:wal-fsync 1000})
            t1 (System/nanoTime)]
        (store/persist-index! db ix)
        (println (format "永続化(flush):        %.1f ms" (/ (- (System/nanoTime) t1) 1e6)))
        (lsm/close db)))
    (println (format "インデックスサイズ:   %.1f KB" (/ (dir-size (File. dir)) 1024.0)))
    (let [db (lsm/open dir {:wal-fsync 1000})
          pf #(store/get-posting db %)
          wf #(store/scan-words db %)
          words (store/all-words db)
          wpf #(store/get-word db %)
          queries (repeatedly 200 #(rand-nth vocab))]
     (println "検索レイテンシ:")
     (bench-search db "Boolean"   #(q/search pf %) queries)
     (bench-search db "Wildcard"  #(q/wildcard-search wf (subs % 0 (min 2 (count %)))) queries)
     (bench-search db "Fuzzy"     #(let [{:keys []} nil]
                                     (require 'my-text-search.fuzzy)
                                     ((resolve 'my-text-search.fuzzy/fuzzy-search) words wpf % 1)) queries)
     (bench-search db "Phrase"    #(q/phrase-search pf %) queries)
     (lsm/close db))))
