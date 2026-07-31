(ns my-text-search.facet)

(defn facet-counts
  [value-fn doc-ids]
  (frequencies (keep value-fn doc-ids)))

(defn facets
  [value-fns doc-ids]
  (into {} (for [[attr vf] value-fns]
             [attr (facet-counts vf doc-ids)])))

(defn sorted-counts
  [counts]
  (sort-by (juxt (comp - val) key) counts))
