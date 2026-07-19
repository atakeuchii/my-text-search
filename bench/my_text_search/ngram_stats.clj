(ns my-text-search.ngram-stats
  (:require [my-text-search.tokenizer :as tok]))

(def corpus
  ["日本酒の醸造は米と水と麹から始まる"
   "純米大吟醸は精米歩合が高い日本酒である"
   "水質が酒の味を大きく左右する"])

(defn stats [n corpus]
  (let [toks (mapcat #(tok/tokenize n %) corpus)]
    {:n n
     :total  (count toks)            ; ポスティング総延長の目安
     :unique (count (distinct toks)) ; term辞書サイズの目安
     :dup-ratio (double (/ (count toks)
                           (max 1 (count (distinct toks)))))}))

(defn -main [& args]
  (let [stats (map #(stats % corpus) [1 2 3])]
    (doseq [s stats]
      (println s))))
