(ns my-text-search.search
  (:require [clojure.string :as str]
            [my-text-search.tokenizer :as tok]
            [my-text-search.query :as q]
            [my-text-search.fuzzy :as fz]))

(defn search
  [sources query & {:keys [op k] :or {op :and k 1}}]
  (let [{:keys [posting-fn word-scan-fn words word-posting-fn]} sources]
    (cond
      (str/ends-with? query "*")
      (q/wildcard-search word-scan-fn (subs query 0 (dec (count query))))
      
      (str/ends-with? query "~")
      (fz/fuzzy-search words word-posting-fn
                       (tok/normalize (subs query 0 (dec (count query)))) k)
      
      :else
      (q/search posting-fn query :op op))))
