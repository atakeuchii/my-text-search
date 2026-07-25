(ns my-text-search.fuzzy-bench
  (:require [my-text-search.fuzzy :as fz]))

;; --- 計測用ヘルパー ---
(defn bench
  "f を warmup 回空回ししてから runs 回計測し、1回あたりの中央値(ms)を返す。"
  [label f & {:keys [warmup runs] :or {warmup 3 runs 7}}]
  (dotimes [_ warmup] (doall (f)))                 ; JIT ウォームアップ
  (let [times (sort (for [_ (range runs)]
                      (let [t0 (System/nanoTime)]
                        (doall (f))
                        (/ (- (System/nanoTime) t0) 1e6))))
        median (nth times (quot runs 2))
        hits   (count (f))]
    (println (format "  %-8s median %.2f ms  (min %.2f / max %.2f, ヒット %d)"
                     label median (first times) (last times) hits))
    median))

;; --- 計測用の辞書を作る ---
;; ノイズ語(クエリと長さが違う多数) + 本命の近い語 数個
(defn make-words [noise-count]
  (doall (concat
          (for [i (range noise-count)] (str "word" i "xyz"))
          ["日本酒" "日木酒" "日本酒造" "日本語"])))

;; --- 実行 ---
(def query "日本酒")
(def k 1)

(defn -main [& args]
  (doseq [n [1000 10000 50000 100000]]
    (let [words (make-words n)]
      (println (format "語数 %d:" (count words)))
      (let [mn (bench "naive"  #(fz/fuzzy-terms-naive words query k))
            mf (bench "枝刈り" #(fz/fuzzy-terms words query k))]
        (println (format "  倍率 naive/枝刈り = %.1fx" (/ mn mf)))
        (println "  結果一致:" (= (set (fz/fuzzy-terms-naive words query k))
                              (set (fz/fuzzy-terms words query k))))))))
